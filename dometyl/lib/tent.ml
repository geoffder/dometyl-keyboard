open! Base
open! Scad_ml

(* FIXME: still need to figure out correct rotation axis that prevents unwanted tilt *)
(* TODO:
   - calculate / estimate height needed for extrusion upward, and the cut, so that
     they aren't just extremely rough "magic" numbers.
   - either remake the screw-holes protrusions from scratch, or alter the existing
     ones to make them the appropriate radius, with countersink as a nice touch.
   - add protusions with insets for feet (place similar to screws, protruding from
     the centre of Wall.t's?). Most places are fair game, other than under a screw
     on the last pinky column I imagin. *)
let make ?(degrees = 30.) ?(z_offset = 0.) ?(screw_height = 2.) (case : _ Case.t) =
  let Plate.{ n_rows; n_cols; _ } = case.plate.config in
  let xy_origin c k =
    Vec3.mul (Columns.key_exn case.plate.columns c k).origin (1., 1., 0.)
  in
  let pivot_pt =
    let ((_, right, _, _) as bbox) = Connect.bounding_box case.connections in
    let _, centre_y = Connect.centre bbox in
    -.right, -.centre_y, 0.
  and pivot_ax =
    Vec3.(normalize (xy_origin (n_cols - 1) (n_rows - 1) <-> xy_origin (n_cols - 1) 0))
  (* and pivot_ax = 0., 1., 0. *)
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
  in
  let trans s =
    Model.quaternion_about_pt
      (Quaternion.make pivot_ax (degrees *. Float.pi /. 180.))
      pivot_pt
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
        [ Model.projection top
          |> Model.offset (`Delta 2.)
          |> Model.linear_extrude ~height:10.
          |> Model.translate (0., 0., -10.)
        ]
    ; Model.difference shell [ cut ]
    ]
