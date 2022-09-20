open! Scad_ml

type bump_loc =
  | Body of Util.idx * [ `N | `E | `S | `W ]
  | Thumb of Util.idx * [ `N | `E | `S | `W ]

let find_bump_wall (walls : Walls.t) = function
  | Body (i, side) -> Util.idx_to_find i @@ Walls.Sides.get walls.body side
  | Thumb (i, side) -> Util.idx_to_find i @@ Walls.Sides.get walls.thumb side

let default_bumps =
  [ Thumb (First, `W)
  ; Thumb (Last, `S)
  ; Body (First, `N)
  ; Body (Idx 3, `N)
  ; Body (Last, `N)
  ; Body (Last, `S)
  ; Body (Idx 2, `S)
  ]

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
  (* let shell = *)
  (*   Scad.sub *)
  (*     Mesh.(to_scad @@ skin_between ~slices:15 outer' outer) *)
  (*     Mesh.( *)
  (*       to_scad *)
  (*       @@ skin_between ~slices:1 (Path3.ztrans (-0.01) inner') (Path3.ztrans 0.01 inner)) *)
  (* in *)
  let shell =
    let w = 12.
    and n = 25 in
    let _w_outer = w /. Path3.length ~closed:true outer
    and _w_outer' = w /. Path3.length ~closed:true outer'
    and _w_inner = w /. Path3.length ~closed:true inner
    and _w_inner' = w /. Path3.length ~closed:true inner' in
    let step = 1. /. Float.of_int (n - 1) in
    let cont_outer = Path3.to_continuous ~closed:true outer
    and cont_outer' = Path3.to_continuous ~closed:true outer'
    and cont_inner = Path3.to_continuous ~closed:true inner
    and cont_inner' = Path3.to_continuous ~closed:true inner' in
    let tilt = 0. in
    let _t_outer = tilt /. Path3.length ~closed:true outer
    and _t_outer' = tilt /. Path3.length ~closed:true outer'
    and _t_inner = tilt /. Path3.length ~closed:true inner
    and _t_inner' = tilt /. Path3.length ~closed:true inner' in
    let f i =
      (* let u = Float.min (1. -. w_outer) (step *. Float.of_int i) in *)
      let u = step *. Float.of_int i in
      let g k =
        let tilt_u t = u +. (t /. 35. *. Float.of_int k) in
        let step_u w t j = mod_float (tilt_u t +. (w /. 35. *. Float.of_int j)) 1. in
        let bot =
          List.concat
            [ List.init 36 (fun j -> cont_outer' (step_u _w_outer' _t_outer' j))
            ; List.rev
              @@ List.init 36 (fun j -> cont_inner' (step_u _w_inner' _t_inner' j))
            ]
        and top =
          List.concat
            [ List.init 36 (fun j -> cont_outer (step_u _w_outer _t_outer j))
            ; List.rev @@ List.init 36 (fun j -> cont_inner (step_u _w_inner _t_inner j))
            ]
        in
        List.map2 (fun a b -> V3.lerp a b (1. /. 35. *. Float.of_int k)) bot top
      in
      (* Mesh.to_scad @@ Mesh.skin_between ~slices:2 bot top *)
      (* Mesh.to_scad @@ Mesh.of_rows (List.init 36 g) *)
      let prune (acc, last) row =
        let min_z = (Path3.bbox last).min.z in
        let valid = List.for_all (fun p -> p.z -. 0.1 > min_z) row in
        if valid then row :: acc, row else acc, last
      in
      let rows = List.init 36 g in
      fst @@ List.fold_left prune ([ List.hd rows ], List.hd rows) (List.tl rows)
      |> List.rev
      |> Mesh.of_rows
      |> Mesh.to_scad
    in
    Scad.union (List.init n f)
  in
  let feet, final_cuts =
    let tilted = Case.rotate ~about rot case |> Case.ztrans foot_thickness in
    let f (bumps, insets) loc =
      match find_bump_wall tilted.walls loc with
      | Some Wall.{ foot; _ } ->
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
            V3.(mid foot.bot_right foot.bot_left *@ v3 1. 1. 0.)
        in
        b.scad :: bumps, Option.get b.cut :: insets
      | None -> bumps, insets
    in
    List.fold_left f ([], clearances) bump_locs
  in
  Scad.difference (Scad.union (shell :: eyelets :: feet)) final_cuts
