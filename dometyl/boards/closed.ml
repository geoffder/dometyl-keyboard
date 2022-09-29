open! Scad_ml
open! Generator

let body_lookups = Splaytyl.body_lookups
let thumb_lookups = Splaytyl.thumb_lookups
let plate_builder = Splaytyl.plate_builder

let wall_builder plate =
  Walls.
    { body =
        auto_body
          ~d1:(`Abs 14.)
          ~d2:10.
          ~n_steps:(`PerZ 1.)
          ~scale:(v2 0.8 0.9)
          ~scale_ez:(v2 0.42 1., v2 1. 1.)
          ~west_lookup:(fun _ -> true)
          ~east_lookup:(fun _ -> true)
          ~north_clearance:0.
          ~south_clearance:0.
          ~side_clearance:0.
          plate
    ; thumb =
        auto_thumb
          ~d1:(`Abs 14.)
          ~d2:8.
          ~n_steps:(`PerZ 1.)
          ~scale:(v2 0.8 0.9)
          ~scale_ez:(v2 0.42 1., v2 1. 1.)
          ~south_lookup:(fun _ -> true)
          ~east_lookup:(fun _ -> false)
          ~west_lookup:(fun _ -> true)
          ~north_clearance:0.
          ~south_clearance:0.
          ~side_clearance:0.
          plate
    }

let base_connector = Connect.closed ~spline_d:0.1
let plate_welder = Plate.column_joins ~body_skip:[ 3, 0 ]
let ports_cutter = Splaytyl.ports_cutter

let build ?right_hand ?hotswap () =
  (* let keyhole = Mx.make_hole ?hotswap () in *)
  let keyhole = Mx.make_hole ?hotswap ~corner:(Path3.Round.chamf (`Cut 0.5)) () in
  (* let keyhole = Mx.make_hole ?hotswap ~corner:(Path3.Round.bez (`Cut 0.5))  () in *)
  Case.make
    ?right_hand
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    keyhole

let bump_locs_right =
  List.map
    (fun p -> Bottom.point (V2.of_tup p))
    [ 105., 5.; 80., -55.; 20., -55.; -25., -55.; -5., 32.; 75., 32. ]
