open! Base
open! Scad_ml
open! Generator

let plate_builder = Plate.make ~n_rows:3 ~n_cols:5

let wall_builder plate =
  Walls.
    { body = Body.make ~n_steps:(`Flat 3) ~clearance:1.5 plate
    ; thumb =
        Thumb.make
          ~south_lookup:(fun _ -> Yes)
          ~east:No
          ~west:Screw
          ~clearance:1.5
          ~n_steps:(`Flat 3)
          plate
    }

let base_connector =
  Connect.skeleton
    ~n_facets:1
    ~height:9.
    ~thumb_height:11.
    ~snake_scale:1.3
    ~snake_d:1.4
    ~cubic_d:2.
    ~cubic_scale:1.
    ~west_link_cubic:false
    ~thumb_cubic_d:4.
    ~thumb_cubic_scale:2.
    ~body_join_steps:3
    ~thumb_join_steps:3
    ~fudge_factor:8.
    ~close_thumb:true
    ~pinky_elbow:false

let plate_welder plate =
  Model.union [ Plate.skeleton_bridges plate; Bridge.cols ~columns:plate.columns 1 2 ]

let ports_cutter = BastardShield.(cutter ~x_off:1. ~y_off:(-1.) (make ()))

let build ?right_hand ?hotswap () =
  Case.make
    ?right_hand
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    (Mx.make_hole ~cap:Caps.sa_r3 ?hotswap ())

let bastard_compare () =
  Model.union
    [ Skeletyl.bastard_skelly
    ; Case.to_scad ~show_caps:false (build ()) |> Model.color ~alpha:0.5 Color.Yellow
    ]
