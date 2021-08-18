open! Base
open! Scad_ml

(* TODO:
   - either remake the screw-holes protrusions from scratch, or alter the existing
     ones to make them the appropriate radius, with countersink as a nice touch.
   - add protusions with insets for feet (place similar to screws, protruding from
     the centre of Wall.t's?). Most places are fair game, other than under a screw
     on the last pinky column I imagin. *)
let make ?(degrees = 30.) ?(z_offset = 0.) ?(screw_height = 2.) (case : _ Case.t) =
  let _, bb_right, _, bb_left = Connect.bounding_box case.connections in
  let rot = 0., degrees *. Float.pi /. 180., 0.
  and pivot_pt = -.bb_right, 0., 0.
  and screwed = Model.projection ~cut:true (Model.translate (0., 0., -0.01) case.scad) in
  let screwless =
    let screws =
      Walls.collect_screws case.walls
      |> List.map ~f:Screw.to_scad
      |> Model.union
      |> Fn.flip Model.difference [ case.connections.scad ]
      |> Model.translate (0., 0., -0.01)
      |> Model.projection ~cut:true
    in
    Model.difference screwed [ screws ]
  and trans s = Model.rotate_about_pt rot pivot_pt s |> Model.translate (0., 0., z_offset)
  and base_height = Vec3.(get_z (rotate_about_pt rot pivot_pt (bb_left, 0., 0.))) in
  let top = trans (Model.linear_extrude ~height:screw_height screwed)
  and shell =
    trans (Model.linear_extrude ~height:0.001 screwless)
    |> Model.projection
    |> Model.linear_extrude ~height:base_height
  in
  let cut =
    let bulked_top =
      Model.offset (`Delta 2.) screwed
      |> Model.linear_extrude ~height:screw_height
      |> trans
    in
    Model.hull [ bulked_top; Model.translate (0., 0., base_height) bulked_top ]
  in
  Model.union
    [ Model.difference
        top
        [ Model.projection top
          |> Model.offset (`Delta 2.)
          |> Model.linear_extrude ~height:10.
          |> Model.translate (0., 0., -10.)
        ]
    ; Model.difference shell [ Model.translate (0., 0., -0.00001) cut ]
    ]
