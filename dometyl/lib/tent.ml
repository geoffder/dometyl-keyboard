open! Base
open! Scad_ml

(* TODO:
   - find rightmost point and rotate about that edge as an axis instead of
   the magic z_offset value stand-in solution.
   - right now the rotation axis is using the key origins on the pinky column. Update
   so that it uses the outer vertices of the last columns wall feet. Should work, and
   be more general.
   - calculate / estimate height needed for extrusion upward, and the cut, so that
     they aren't just extremely rough "magic" numbers.
   - add protusions with insets for feet *)
let make ?(degrees = 30.) ?(z_offset = 33.) ?(screw_height = 4.) (case : _ Case.t) =
  let xy_origin c k =
    Vec3.mul (Columns.key_exn case.plate.columns c k).origin (1., 1., 0.)
  in
  let centre = xy_origin 2 1 in
  let pivot_ax = Vec3.(normalize (xy_origin 4 2 <-> xy_origin 4 0))
  and screwed = Model.projection ~cut:true case.scad in
  let screwless =
    let screws =
      Walls.collect_screws case.walls
      |> List.map ~f:Screw.to_scad
      |> Model.union
      |> Model.projection ~cut:true
    in
    Model.difference screwed [ screws ]
  in
  let trans s =
    Model.quaternion_about_pt
      (Quaternion.make pivot_ax (degrees *. Float.pi /. 180.))
      (Vec3.negate centre)
      s
    |> Model.translate (0., 0., z_offset -. screw_height)
  in
  let top = trans (Model.linear_extrude ~height:screw_height screwed)
  and shell =
    trans (Model.linear_extrude ~height:0.001 screwless)
    |> Model.projection
    |> Model.linear_extrude ~height:100.
  in
  let cut = Model.hull [ top; Model.translate (0., 0., 200.) top ] in
  Model.union
    [ Model.difference
        top
        [ Model.linear_extrude ~height:10. (Model.projection top)
          |> Model.translate (0., 0., -10.)
        ]
    ; Model.difference shell [ cut ]
    ]
