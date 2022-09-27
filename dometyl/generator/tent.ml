open! Scad_ml

type style =
  | Solid
  | Prison of
      { n_pillars : int option
      ; width : float option
      ; tilt : float option
      ; tilt_ez : (v2 * v2) option
      ; fn : int option
      ; slices : Wall.Steps.t option
      ; phase_shift : float option
      ; corner : Path3.Round.corner option
      ; corner_fn : int option
      ; fillet_d : [ `Abs of float | `Rel of float ] option
      ; fillet_w : float option
      }

let prison
    ?n_pillars
    ?width
    ?tilt
    ?tilt_ez
    ?fn
    ?slices
    ?phase_shift
    ?fillet_d
    ?fillet_w
    ?corner
    ?corner_fn
    ()
  =
  Prison
    { n_pillars
    ; width
    ; tilt
    ; tilt_ez
    ; fn
    ; slices
    ; phase_shift
    ; corner
    ; corner_fn
    ; fillet_d
    ; fillet_w
    }

let default_bumps = [ 0.; 0.15; 0.3; 0.4; 0.5; 0.77; 0.9 ]

let solid_shell bot_outer bot_inner top_outer top_inner =
  let outer = Mesh.skin_between ~slices:3 bot_outer top_outer
  and inner =
    Mesh.skin_between
      ~slices:3
      (Path3.ztrans (-0.01) bot_inner)
      (Path3.ztrans 0.01 top_inner)
  in
  Scad.sub (Mesh.to_scad outer) (Mesh.to_scad inner)

let prison_shell
    ?(n_pillars = 30)
    ?(width = 12.)
    ?(tilt = 15.)
    ?(tilt_ez = v2 0.42 0., v2 0.58 1.)
    ?(fn = 18)
    ?(slices = `PerZ 1.)
    ?(phase_shift = 0.)
    ?(fillet_d = `Rel 0.2)
    ?(fillet_w = 0.25)
    ?corner
    ?corner_fn
    bot_outer
    bot_inner
    top_outer
    top_inner
  =
  let slices = Wall.Steps.to_int slices in
  let pillar_step = 1. /. Float.of_int n_pillars
  and fn_step = 1. /. Float.of_int (fn - 1) in
  let tilt_ez = Easing.make (fst tilt_ez) (snd tilt_ez) in
  let len_outer = Path3.length ~closed:true top_outer
  and len_outer' = Path3.length ~closed:true bot_outer in
  let w_outer = width /. len_outer
  and w_outer' = width /. len_outer' in
  let cont_outer = Path3.to_continuous ~closed:true top_outer
  and cont_outer' = Path3.to_continuous ~closed:true bot_outer
  and cont_inner = Path3.to_continuous ~closed:true top_inner
  and cont_inner' = Path3.to_continuous ~closed:true bot_inner in
  let t_outer = tilt /. len_outer
  and t_outer' = tilt /. len_outer' in
  let fillet_ez =
    match fillet_d with
    | `Rel d -> Fun.const (Easing.make (v2 d 1.) (v2 d 1.))
    | `Abs d ->
      fun z ->
        let d = Float.min (d /. z) 1. in
        Easing.make (v2 d 1.) (v2 d 1.)
  in
  let prune (acc, last) row =
    let min_z = (Path3.bbox last).min.z in
    let valid = List.for_all (fun p -> p.z > min_z) row in
    if valid then row :: acc, row else acc, last
  and rounder out_edge in_edge =
    match corner with
    | Some c ->
      let cs = Some c :: Util.fold_init (fn - 2) (fun _ acc -> None :: acc) [ Some c ] in
      List.append (List.combine out_edge cs) (List.combine in_edge cs)
      |> Path3.Round.mix
      |> Path3.roundover ?fn:corner_fn ~overrun:`Fix
    | None -> List.append out_edge in_edge
  in
  let pillar i =
    let u = (pillar_step *. Float.of_int i) +. phase_shift in
    let z = (cont_outer u).z in
    let slices = slices z in
    let slice_step = 1. /. Float.of_int (slices - 1) in
    let profile k =
      let step_u w t j =
        let ku = Float.of_int k *. slice_step in
        let tilted = u +. (t *. tilt_ez ku) in
        let fillet = fillet_w *. fillet_ez z (if k > slices / 2 then 1. -. ku else ku) in
        let start = (fillet /. 2. *. w) +. tilted in
        let u = start +. (w *. (1. -. fillet) *. (Float.of_int j *. fn_step)) in
        mod_float u 1. (* wrap around *)
      in
      let lerpn, bot =
        let outer = List.init fn (fun j -> cont_outer' (step_u w_outer' t_outer' j)) in
        let closest_u = Path3.continuous_closest_point ~closed:true cont_inner' in
        let u0 = closest_u (Util.last outer) in
        let u' = closest_u (List.hd outer) in
        let lerpn =
          if u0 > u'
          then Math.lerpn u0 u'
          else
            fun n ->
            let diff = 1. -. u' +. u0 in
            let s = diff /. Float.of_int (n - 1) in
            List.init n (fun i -> mod_float ((Float.of_int (n - i) *. s) +. u') 1.)
        in
        lerpn, rounder outer (List.map cont_inner' (lerpn fn))
      in
      let top =
        let outer = List.init fn (fun j -> cont_outer (step_u w_outer t_outer j)) in
        rounder outer (List.map cont_inner (lerpn fn))
      in
      let subdiv =
        let n = Int.max (List.length bot) (List.length top) in
        Path3.subdivide ~closed:true ~freq:(`N (n, `ByLen))
      in
      Path3.lerp (subdiv bot) (subdiv top) (Float.of_int k *. slice_step)
    in
    let rows = List.init slices profile in
    fst @@ List.fold_left prune ([ List.hd rows ], List.hd rows) (List.tl rows)
    |> List.rev
    |> Mesh.skin ~slices:(`Flat 0)
    |> Mesh.to_scad
  in
  let z = (Path3.bbox top_outer).min.z in
  let bot =
    let outer = Mesh.skin_between ~slices:1 bot_outer (Path3.ztrans z bot_outer)
    and inner =
      Mesh.skin_between
        ~slices:1
        (Path3.ztrans (-0.01) bot_inner)
        (Path3.ztrans (z +. 0.01) bot_inner)
    in
    Scad.sub (Mesh.to_scad outer) (Mesh.to_scad inner)
  and top =
    let outer = Mesh.skin_between ~slices:1 (Path3.ztrans (-.z) top_outer) top_outer
    and inner =
      Mesh.skin_between
        ~slices:1
        (Path3.ztrans (-.z -. 0.01) top_inner)
        (Path3.ztrans 0.01 top_inner)
    in
    Scad.sub (Mesh.to_scad outer) (Mesh.to_scad inner)
  in
  Scad.union (bot :: top :: List.init n_pillars pillar)

let shell_of_style = function
  | Solid -> solid_shell
  | Prison
      { n_pillars
      ; width
      ; tilt
      ; tilt_ez
      ; fn
      ; slices
      ; phase_shift
      ; corner
      ; corner_fn
      ; fillet_d
      ; fillet_w
      } ->
    prison_shell
      ?n_pillars
      ?width
      ?tilt
      ?tilt_ez
      ?fn
      ?slices
      ?phase_shift
      ?corner
      ?corner_fn
      ?fillet_d
      ?fillet_w

let make
    ?(degrees = 20.)
    ?fastener
    ?(foot_thickness = 2.4)
    ?(foot_rad = 6.)
    ?(bumpon_rad = 5.5)
    ?(bumpon_inset = 0.8)
    ?(bump_locs = default_bumps)
    ?(style = prison ())
    (case : Case.t)
  =
  let bb_pinky, rot_sign =
    let V3.{ min = { x = bb_left; _ }; max = { x = bb_right; _ } } =
      Path3.bbox case.connections.outline
    and pinky_home =
      let n = case.plate.config.n_body_cols - 1 in
      (Columns.key_exn
         case.plate.body
         n
         (Int.of_float @@ case.plate.config.body_centres n) )
        .origin
    in
    if V3.(norm (pinky_home -@ v3 bb_right 0. 0.))
       < V3.(norm (pinky_home -@ v3 bb_left 0. 0.))
    then bb_right, 1.
    else bb_left, -1.
  and screws = Walls.collect_screws case.Case.walls in
  let outline = Connect.outline_2d case.connections
  and inline = Connect.inline_2d case.connections in
  let eyelet_config = (List.hd screws).config in
  let fastener =
    match fastener with
    | None ->
      ( match eyelet_config with
      | { hole = Through; _ } -> Eyelet.screw_fastener ()
      | _ -> SameMagnet )
    | Some fastener -> fastener
  and rot = v3 0. (Math.rad_of_deg degrees *. rot_sign) 0.
  and about = v3 bb_pinky 0. 0. in
  let hole_cut, hole_height, clearances =
    let magnetize rad h thickness =
      let hole =
        Scad.union
          [ Scad.ztrans (thickness -. h) @@ Scad.cylinder ~fn:32 ~height:(h +. 0.01) rad
          ; Scad.cylinder ~fn:32 ~height:(thickness -. h +. 0.01) (rad /. 2.)
          ]
        |> Scad.ztrans (-0.005)
      in
      hole, thickness, []
    in
    match fastener with
    | Screw { head_rad; shaft_rad; sink; height; clearance } ->
      let head_disc = Scad.circle head_rad in
      let hole =
        ( match sink with
        | Counter ->
          let s = shaft_rad /. head_rad in
          Scad.extrude ~height:(height +. 0.01) ~scale:(v2 s s) head_disc
        | Pan inset ->
          Scad.union
            [ Scad.extrude ~height:(inset +. 0.01) head_disc
            ; Scad.cylinder ~fn:32 ~height:(height +. 0.01) shaft_rad
            ] )
        |> Scad.ztrans (-0.005)
      and clearances =
        match clearance with
        | Some height ->
          let cyl = Scad.ztrans (-.height) @@ Scad.extrude ~height head_disc in
          List.map (fun Eyelet.{ centre; _ } -> Scad.translate centre cyl) screws
        | None -> []
      in
      hole, height, clearances
    | SameMagnet ->
      let Eyelet.{ inner_rad; thickness; hole; _ } = eyelet_config in
      let h =
        match hole with
        | Eyelet.Inset { depth; _ } -> depth
        | _ -> failwith "Case eyelet expected to be magnet inset."
      in
      magnetize inner_rad h thickness
    | Magnet { rad; thickness } -> magnetize rad thickness (thickness +. 1.4)
  in
  (* raise top above the feet or the tilted eyelets (avoid clipping) *)
  let rise = Float.max foot_thickness (V3.rotate rot (v3 0. 0. hole_height)).z in
  let place_scad s = Scad.ztrans rise @@ Scad.rotate ~about rot s
  and place_v3 p = V3.ztrans rise @@ V3.rotate ~about rot (V3.of_v2 p) in
  let eyelets =
    List.map
      (fun Eyelet.{ centre; config = { inner_rad; outer_rad; hole; _ }; scad; _ } ->
        let proj = Scad.projection scad in
        ( match hole with
        | Inset _ -> proj
        | Through ->
          let r = Math.lerp inner_rad outer_rad 0.2 in
          Scad.(union [ translate (V2.of_v3 centre) (circle ~fn:16 r); proj ]) )
        |> Scad.extrude ~height:hole_height
        |> Fun.flip Scad.sub (Scad.translate centre hole_cut)
        |> Scad.ztrans (-.hole_height) )
      screws
    |> Scad.union
    |> place_scad
  in
  let outer = List.map place_v3 outline
  and inner = List.map place_v3 inline in
  let outer' = List.map (fun { x; y; z = _ } -> v3 x y 0.) outer
  and inner' = List.map (fun { x; y; z = _ } -> v3 x y 0.) inner in
  let shell = shell_of_style style outer' inner' outer inner in
  let feet, final_cuts =
    let f (bumps, insets) loc =
      let b =
        Eyelet.place
          ~bury:0.4
          ~config:
            Eyelet.
              { outer_rad = foot_rad
              ; inner_rad = bumpon_rad
              ; thickness = foot_thickness
              ; hole = inset bumpon_inset
              }
          ~inline:inner'
          ~outline:outer'
          (`U loc)
      in
      b.scad :: bumps, Option.get b.cut :: insets
    in
    List.fold_left f ([], List.map place_scad clearances) bump_locs
  in
  Scad.difference (Scad.union (shell :: eyelets :: feet)) final_cuts
