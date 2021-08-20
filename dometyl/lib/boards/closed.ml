open! Base
open! Scad_ml

let wall_builder plate =
  Walls.
    { body =
        Body.make
          ~west_lookup:(fun i -> if i = 0 then Screw else Yes)
          ~east_lookup:(fun _ -> Yes)
          ~n_steps:(`PerZ 6.)
          plate
    ; thumb =
        Thumb.make
          ~east:No
          ~south_lookup:(fun i -> if i = 1 then Screw else Yes)
          ~n_steps:(`PerZ 6.)
          plate
    }

let base_connector = Connect.closed ~n_steps:4
let plate_welder = Plate.column_joins

let build () =
  let keyhole = KeyHole.make ~cap:Caps.sa_r3 Mx.hole_config in
  let plate = Plate.make ~n_rows:3 ~n_cols:5 ~clearance:Mx.plate_clearance keyhole in
  Case.make ~plate_welder ~wall_builder ~base_connector plate
