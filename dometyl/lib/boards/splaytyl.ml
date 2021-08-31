open! Base
open! Scad_ml
open! Generator

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
    ~height:7.
    ~thumb_height:11.
    ~snake_scale:1.3
    ~snake_d:1.4
    ~cubic_d:2.
    ~cubic_scale:1.
    ~thumb_cubic_d:1.
    ~thumb_cubic_scale:1.25
    ~body_join_steps:3
    ~thumb_join_steps:3
    ~fudge_factor:8.
    ~close_thumb:true
    ~close_pinky:false

let plate_welder plate =
  Model.union [ Plate.skeleton_bridges plate; Bridge.cols ~columns:plate.columns 1 2 ]

let ports_cutter = Ports.make

let build () =
  (* let keyhole = Mx.make_hole ~cap:Caps.sa_r3 ~hotswap:`South () in *)
  let keyhole = Mx.make_hole ~cap:Caps.sa_r3 () in
  let plate = Plate.make ~n_rows:3 ~n_cols:5 keyhole in
  Case.make ~plate_welder ~wall_builder ~base_connector ~ports_cutter plate

let bastard_compare () =
  Model.union
    [ Skeletyl.bastard_skelly
    ; Case.to_scad ~show_caps:false (build ()) |> Model.color ~alpha:0.5 Color.Yellow
    ]
