open! Base
open! Scad_ml
open! Generator

let lookups =
  let offset = function
    | 2 -> 0., 3.5, -6. (* middle *)
    | 3 -> 1., -2.5, 0.5 (* ring *)
    | i when i >= 4 -> 1., -18., 8.5 (* pinky *)
    | 0 -> -2., -1., 7.
    | _ -> 0., -1., 1.5
  and curve = function
    | i when i = 3 ->
      Curvature.(curve ~well:(spec ~radius:37. (Float.pi /. 4.5)) ()) (* ring *)
    | i when i > 3 ->
      Curvature.(curve ~well:(spec ~radius:35. (Float.pi /. 4.1)) ()) (* pinky *)
    | i when i = 0 ->
      Curvature.(
        curve ~well:(spec ~tilt:(Float.pi /. 6.9) ~radius:45. (Float.pi /. 6.)) ())
    | _ -> Curvature.(curve ~well:(spec ~radius:45.5 (Float.pi /. 6.3)) ())
  and splay = function
    | i when i = 3 -> Float.pi /. -25. (* ring *)
    | i when i >= 4 -> Float.pi /. -11. (* pinky *)
    | _ -> 0.
  and rows _ = 3 in
  Plate.Lookups.make ~offset ~curve ~splay ~rows ()

let plate_builder =
  Plate.make ~n_cols:5 ~lookups ~caps:Caps.SA.row ~thumb_caps:Caps.MT3.thumb_1u

let wall_builder plate =
  Walls.
    { body =
        Body.make
          ~n_steps:(`Flat 3)
          ~north_clearance:1.5
          ~south_clearance:1.5
          ~side_clearance:1.5
          plate
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
    ~snake_scale:1.
    ~snake_d:1.2
    ~cubic_d:2.
    ~cubic_scale:1.5
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
    (Mx.make_hole ?hotswap ())

let bastard_compare () =
  Model.union
    [ Skeletyl.bastard_skelly
    ; Case.to_scad ~show_caps:false (build ()) |> Model.color ~alpha:0.5 Color.Yellow
    ]
