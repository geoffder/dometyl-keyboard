open! Base
open! Scad_ml

let bumpon ?(n_steps = 5) ~outer_rad ~inner_rad ~thickness ~inset foot =
  let Points.{ top_left; top_right; bot_left; bot_right; centre } =
    Points.map ~f:(Vec3.mul (1., 1., 0.)) foot
  in
  let normal = Vec3.(centre <-> mean [ top_left; top_right ] |> normalize) in
  let base_centre = Vec3.(map (( *. ) 0.5) (top_left <+> top_right) |> mul (1., 1., 0.))
  and hole_offset = Vec3.map (( *. ) outer_rad) normal in
  let centre = Vec3.(base_centre <+> hole_offset) in
  let circ = Scad.circle ~fn:32 outer_rad |> Scad.translate centre
  and swoop p =
    let rad_offset = Vec3.(map (( *. ) outer_rad) (normalize (p <-> base_centre))) in
    centre
    :: base_centre
    :: p
    :: Bezier.curve
         ~n_steps
         (Bezier.quad_vec3
            ~p1:p
            ~p2:Vec3.(mean [ base_centre <+> rad_offset; p ])
            ~p3:Vec3.(centre <+> rad_offset) )
    |> List.map ~f:Vec3.to_vec2
    |> Scad.polygon
  in
  let bump =
    Scad.union
      [ circ
      ; swoop (Vec3.mul bot_left (1., 1., 0.))
      ; swoop (Vec3.mul bot_right (1., 1., 0.))
      ]
    |> Scad.linear_extrude ~height:thickness
  and inset_cut = Scad.cylinder ~fn:16 inner_rad inset |> Scad.translate centre in
  bump, inset_cut

(* TODO:
   - make the positioning of bumpons more flexible, right now just using wall
     positions, as with screw placement on the case.
   - paramaterizable / smarter bumpon placement, right now the important pinky
     position is obscuring the screw hole above.
*)
let make
    ?(degrees = 30.)
    ?(z_offset = 0.)
    ?(screw_height = 2.)
    ?(outer_screw_rad = 4.1)
    ?(inner_screw_rad = 2.0)
    ?(foot_thickness = 2.)
    ?(foot_rad = 5.8)
    ?(bumpon_rad = 5.)
    ?(bumpon_inset = 0.5)
    (case : _ Case.t)
  =
  let bb_index, bb_pinky, rot_sign =
    let _, bb_right, _, bb_left = Util.bounding_box case.connections.outline
    and pinky_home =
      let n = case.plate.config.n_cols - 1 in
      (Columns.key_exn case.plate.columns n (case.plate.config.row_centres n)).origin
    in
    if Float.(
         Vec3.(norm (pinky_home <-> (bb_right, 0., 0.)))
         < Vec3.(norm (pinky_home <-> (bb_left, 0., 0.))))
    then bb_left, bb_right, 1.
    else bb_right, bb_left, -1.
  and screws = Walls.collect_screws case.Case.walls
  and perimeter =
    Scad.difference
      (Scad.polygon (Connect.outline_2d case.connections))
      [ Scad.polygon (Connect.inline_2d case.connections) ]
  in
  let rot = 0., Util.deg_to_rad degrees *. rot_sign, 0.
  and pivot_pt = -.bb_pinky, 0., 0. in
  let screws_filled =
    let hole_fills =
      List.map
        ~f:(fun Screw.{ centre; config = { inner_rad; _ }; scad; _ } ->
          Scad.union
            [ Scad.translate centre (Scad.circle inner_rad); Scad.projection scad ] )
        screws
    in
    Scad.union (perimeter :: hole_fills)
  and trans s = Scad.rotate_about_pt rot pivot_pt s |> Scad.translate (0., 0., z_offset)
  and base_height = Vec3.(get_z (rotate_about_pt rot pivot_pt (bb_index, 0., 0.))) in
  let top =
    let screw_hole =
      Scad.circle outer_screw_rad
      |> Scad.linear_extrude
           ~height:screw_height
           ~scale:(inner_screw_rad /. outer_screw_rad)
    in
    Scad.difference
      (Scad.linear_extrude ~height:screw_height screws_filled)
      (List.map ~f:(fun Screw.{ centre; _ } -> Scad.translate centre screw_hole) screws)
    |> trans
  and shell =
    trans (Scad.linear_extrude ~height:0.001 perimeter)
    |> Scad.projection
    |> Scad.linear_extrude ~height:base_height
  in
  let cut =
    let bulked_top =
      Scad.offset (`Delta 2.) screws_filled
      |> Scad.linear_extrude ~height:screw_height
      |> trans
    in
    Scad.hull [ bulked_top; Scad.translate (0., 0., base_height) bulked_top ]
  in
  let feet, insets =
    let tilted =
      Case.rotate_about_pt rot pivot_pt case |> Case.translate (0., 0., z_offset)
    in
    let top_left =
      let%bind.Option c = Map.find tilted.walls.body.cols 0 in
      c.north
    and top_ring =
      let%bind.Option c = Map.find tilted.walls.body.cols 3 in
      c.north
    and bot_mid =
      let%bind.Option c = Map.find tilted.walls.body.cols 2 in
      c.south
    and bot_left = tilted.walls.thumb.sides.west
    and right =
      match Map.max_elt tilted.walls.body.cols with
      | Some (_, c) -> [ c.north; c.south ]
      | None        -> []
    and f (bumps, insets) Wall.{ foot; _ } =
      let bump, inset =
        bumpon
          ~outer_rad:foot_rad
          ~inner_rad:bumpon_rad
          ~thickness:foot_thickness
          ~inset:bumpon_inset
          foot
      in
      bump :: bumps, inset :: insets
    in
    let feet, insets =
      bot_mid :: bot_left :: top_left :: top_ring :: right
      |> List.filter_opt
      |> List.fold ~init:([], []) ~f
    in
    Scad.union feet, insets
  in
  Scad.difference
    (Scad.union
       [ Scad.difference
           top
           [ Scad.projection top
             |> Scad.offset (`Delta 2.)
             |> Scad.linear_extrude ~height:10.
             |> Scad.translate (0., 0., -10.)
           ]
       ; Scad.difference shell [ Scad.translate (0., 0., -0.00001) cut ]
       ; feet
       ] )
    insets
