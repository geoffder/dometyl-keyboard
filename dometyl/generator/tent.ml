open! Scad_ml

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

let prison_shell bot_outer bot_inner top_outer top_inner =
  let w = 10.
  and n = 30 in
  let step = 1. /. Float.of_int n in
  let tilt = 15. in
  let w_fn = 24
  and t_fn = 24 in
  let tilt_ez = Easing.make (v2 0.42 0.) (v2 0.58 1.) in
  let sliver = 0.15 in
  let phase_shift = 0.0 in
  let len_outer = Path3.length ~closed:true top_outer
  and len_outer' = Path3.length ~closed:true bot_outer in
  let w_outer = w /. len_outer
  and w_outer' = w /. len_outer' in
  let cont_outer = Path3.to_continuous ~closed:true top_outer
  and cont_outer' = Path3.to_continuous ~closed:true bot_outer
  and cont_inner = Path3.to_continuous ~closed:true top_inner
  and cont_inner' = Path3.to_continuous ~closed:true bot_inner in
  let t_outer = tilt /. len_outer
  and t_outer' = tilt /. len_outer' in
  let closest_u cont (u, p) =
    let ps =
      List.init 100 (fun i ->
          let u = u -. 0.05 +. (Float.of_int i *. 0.1 /. 99.) in
          let u = if u < 0. then 1. +. u else u in
          u, cont u )
    in
    let u0, p0 = List.hd ps in
    snd
    @@ List.fold_left
         (fun (d, closest) (u', p') ->
           let d' = V3.distance p p' in
           if d' < d then d', u' else d, closest )
         (V3.distance p0 p, u0)
         ps
  in
  let prune (acc, last) row =
    let min_z = (Path3.bbox last).min.z in
    let valid = List.for_all (fun p -> p.z > min_z) row in
    if valid then row :: acc, row else acc, last
  in
  (* let corner = Some (Path3.Round.bez (`Cut 0.05)) in *)
  (* let corner = Some (Path3.Round.chamf (`Cut 0.1)) in *)
  let corner = None in
  let corner_fn = Some 6 in
  let rounder out_edge in_edge =
    match corner with
    | Some corner ->
      let corners =
        Some corner
        :: Util.fold_init (w_fn - 2) (fun _ acc -> None :: acc) [ Some corner ]
      in
      let zip a b = List.map2 (fun a b -> a, b) a b in
      List.append (zip out_edge corners) (zip in_edge corners)
      |> Path3.Round.mix
      |> Path3.roundover ?fn:corner_fn
    | None -> List.append out_edge in_edge
  in
  let pillar i =
    let u = (step *. Float.of_int i) +. phase_shift in
    let profile k =
      let step_u w t j =
        let tilted = u +. (t *. tilt_ez (Float.of_int k /. Float.of_int (t_fn - 1))) in
        mod_float (tilted +. (w /. Float.of_int (w_fn - 1) *. Float.of_int j)) 1.
      in
      let lerpn, bot =
        let outer =
          List.init w_fn (fun j ->
              let su = step_u w_outer' t_outer' j in
              su, cont_outer' su )
        in
        let u0 = closest_u cont_inner' (Util.last outer) in
        let u' = closest_u cont_inner' (List.hd outer) in
        let lerpn =
          if u0 > u'
          then Math.lerpn u0 u'
          else
            fun n ->
            let diff = 1. -. u' +. u0 in
            let s = diff /. Float.of_int (n - 1) in
            List.init n (fun i -> mod_float ((Float.of_int (n - i) *. s) +. u') 1.)
        in
        lerpn, rounder (List.map snd outer) (List.map cont_inner' (lerpn w_fn))
      in
      let top =
        let outer = List.init w_fn (fun j -> cont_outer (step_u w_outer t_outer j)) in
        rounder outer (List.map cont_inner (lerpn w_fn))
      in
      List.map2
        (fun a b -> V3.lerp a b (1. /. Float.of_int (t_fn - 1) *. Float.of_int k))
        bot
        top
    in
    let rows = List.init t_fn profile in
    fst @@ List.fold_left prune ([ List.hd rows ], List.hd rows) (List.tl rows)
    |> List.rev
    |> Mesh.of_rows
    |> Mesh.to_scad
  in
  let rel_sliver = false in
  let slabs =
    if rel_sliver
    then (
      let bot =
        let outer =
          Mesh.skin_between
            ~slices:1
            bot_outer
            (List.map2 (fun a b -> V3.lerp a b sliver) bot_outer top_outer)
        and inner =
          Mesh.skin_between
            ~slices:1
            (Path3.ztrans (-0.01) bot_inner)
            (List.map2 (fun a b -> V3.lerp a b (sliver +. 0.01)) bot_inner top_inner)
        in
        Scad.sub (Mesh.to_scad outer) (Mesh.to_scad inner)
      and top =
        let outer =
          Mesh.skin_between
            ~slices:1
            (List.map2 (fun a b -> V3.lerp a b (1. -. sliver)) bot_outer top_outer)
            top_outer
        and inner =
          Mesh.skin_between
            ~slices:1
            (List.map2
               (fun a b -> V3.lerp a b (1. -. sliver -. 0.01))
               bot_inner
               top_inner )
            (Path3.ztrans 0.01 top_inner)
        in
        Scad.sub (Mesh.to_scad outer) (Mesh.to_scad inner)
      in
      Scad.add bot top )
    else (
      let sliver = Float.max 1.5 (Path3.bbox top_outer).min.z in
      let bot =
        let outer = Mesh.skin_between ~slices:1 bot_outer (Path3.ztrans sliver bot_outer)
        and inner =
          Mesh.skin_between
            ~slices:1
            (Path3.ztrans (-0.01) bot_inner)
            (Path3.ztrans (sliver +. 0.01) bot_inner)
        in
        Scad.sub (Mesh.to_scad outer) (Mesh.to_scad inner)
      and top =
        let outer =
          Mesh.skin_between ~slices:1 (Path3.ztrans (-.sliver) top_outer) top_outer
        and inner =
          Mesh.skin_between
            ~slices:1
            (Path3.ztrans (-.sliver -. 0.01) top_inner)
            (Path3.ztrans 0.01 top_inner)
        in
        Scad.sub (Mesh.to_scad outer) (Mesh.to_scad inner)
      in
      Scad.add bot top )
  in
  Scad.union (slabs :: List.init n pillar)

(* TODO: More flexible placement of bumpons using inline, rather than wall positions. *)
let make
    ?(degrees = 20.)
    ?fastener
    ?(foot_thickness = 2.4)
    ?(foot_rad = 6.)
    ?(bumpon_rad = 5.5)
    ?(bumpon_inset = 0.8)
    ?(bump_locs = default_bumps)
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
  let place s = Scad.rotate ~about rot s |> Scad.ztrans foot_thickness in
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
          List.map (fun Eyelet.{ centre; _ } -> place @@ Scad.translate centre cyl) screws
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
    |> place
  in
  let place p = V3.ztrans foot_thickness @@ V3.rotate ~about rot (V3.of_v2 p) in
  let outer = List.map place outline
  and inner = List.map place inline in
  let outer' = List.map (fun { x; y; z = _ } -> v3 x y 0.) outer
  and inner' = List.map (fun { x; y; z = _ } -> v3 x y 0.) inner in
  let prison = true in
  let shell =
    if prison
    then prison_shell outer' inner' outer inner
    else solid_shell outer' inner' outer inner
  in
  let feet, final_cuts =
    let cont_inner' = Path3.to_continuous ~closed:true inner' in
    let f (bumps, insets) loc =
      let b =
        Connect.place_eyelet
          ~bury:0.4
          ~eyelet_config:
            Eyelet.
              { outer_rad = foot_rad
              ; inner_rad = bumpon_rad
              ; thickness = foot_thickness
              ; hole = inset bumpon_inset
              }
          ~relocate:true
          ~inline:inner'
          ~outline:outer'
          (cont_inner' loc)
      in
      b.scad :: bumps, Option.get b.cut :: insets
    in
    List.fold_left f ([], clearances) bump_locs
  in
  Scad.difference (Scad.union (shell :: eyelets :: feet)) final_cuts
