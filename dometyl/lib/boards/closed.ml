open! Base
open! Scad_ml
open! Generator

let plate_builder = Plate.make ~n_rows:3 ~n_cols:5

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
let ports_cutter = Ports.make ()

let build ?hotswap () =
  let keyhole = Mx.make_hole ~cap:Caps.sa_r3 ?hotswap () in
  Case.make
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    keyhole
