open! Scad_ml
open! Generator

let body_lookups = Splaytyl.body_lookups
let thumb_lookups = Splaytyl.thumb_lookups
let plate_builder = Splaytyl.plate_builder

let wall_builder plate =
  Walls.
    { body =
        auto_body
          ~west_lookup:(fun i -> if i = 0 then Eye else Yes)
          ~east_lookup:(fun _ -> Yes)
          ~n_steps:(`PerZ 6.)
          ~north_clearance:2.5
          ~south_clearance:2.5
          ~side_clearance:1.5
          plate
    ; thumb =
        auto_thumb
          ~south_lookup:(fun _ -> Yes)
          ~east_lookup:(fun _ -> No)
          ~west_lookup:(fun _ -> Eye)
          ~n_steps:(`Flat 4)
          ~north_clearance:3.
          ~south_clearance:3.
          ~side_clearance:3.
          plate
    }

let base_connector = Connect.closed
let plate_welder = Plate.column_joins
let ports_cutter = Splaytyl.ports_cutter

let build ?right_hand ?hotswap () =
  let keyhole = Mx.make_hole ?hotswap () in
  Case.make
    ?right_hand
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    keyhole
