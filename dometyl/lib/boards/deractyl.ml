open! Base
open! Scad_ml
open! Generator
open! Infix

let lookups =
  let offset = function
    | 0 -> -2.5, -5., 7.5 (* outer index *)
    | 2 -> 0., 3., -5.5 (* middle *)
    | 3 -> 0., 0., 0. (* ring *)
    | 4 -> 0., -12., 9.5 (* pinky *)
    | 5 -> 5., -14., 21. (* outer pinky *)
    | _ -> 0., -5., 1.
  and curve = function
    | i when i = 0 ->
      Curvature.(
        curve ~well:(spec ~tilt:(Float.pi /. 6.75) ~radius:57.5 (Float.pi /. 8.)) ())
    | i when i = 5 ->
      let f = function
        | 3 ->
          KeyHole.quaternion_about_origin (Float.pi /. 128.)
          >> KeyHole.translate (-0.5, -0.5, 1.)
        | _ -> Fn.id
      in
      Curvature.(
        post_tweak ~well:(spec ~tilt:(Float.pi /. -4.) ~radius:58. (Float.pi /. 8.)) f)
    | _ -> Curvature.(curve ~well:(spec ~radius:60. (Float.pi /. 8.)) ())
  and swing = function
    | _ -> 0.
  and splay = function
    | _ -> 0.
  in
  Plate.Lookups.make ~offset ~curve ~swing ~splay ()

let plate_builder =
  Plate.make
    ~n_rows:4
    ~n_cols:6
    ~spacing:1.
    ~tent:(Float.pi /. 12.)
    ~thumb_offset:(7., -52., 11.5)
    ~thumb_angle:Float.(0., pi /. -3.8, pi /. 6.5)
    ~thumb_curve:
      Curvature.(
        curve
          ~well:{ angle = Float.pi /. 8.; radius = 50.; tilt = 0. }
          ~fan:{ angle = Float.pi /. 28.; radius = 200.; tilt = 0. }
          ())
    ~lookups

let wall_builder plate =
  Walls.
    { body =
        Body.make
          ~west_lookup:(fun i -> if i = 1 then Screw else Yes)
          ~east_lookup:(fun i -> if i = 1 then Screw else Yes)
          ~n_steps:(`Flat 5)
          ~n_facets:2
          ~clearance:4.
          plate
    ; thumb =
        Thumb.make
          ~east:No
          ~north_lookup:(fun _ -> No)
          ~south_lookup:(fun i -> if i = 1 then Screw else Yes)
          ~n_steps:(`Flat 5)
          ~n_facets:4
          plate
    }

let plate_welder = Plate.column_joins

let base_connector =
  Connect.closed
    ~join_west:true
    ~n_steps:5
    ~snake_d:2.
    ~snake_scale:3.
    ~snake_height:10.
    ~snake_steps:6
    ~cubic_d:3.
    ~cubic_scale:0.25
    ~cubic_height:15.

let build ?hotswap ~ports_cutter () =
  let keyhole = Mx.make_hole ?hotswap ~cap:Caps.sa_r3 () in
  Case.make
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    keyhole

let ports_build ?hotswap = build ?hotswap ~ports_cutter:(Ports.make ())
let carbon_x = 0.
let carbon_y = -2.5
let carbon_z = Float.pi /. 30.

let carbon_build ?hotswap =
  build
    ?hotswap
    ~ports_cutter:
      (Ports.carbonfet_holder ~x_off:carbon_x ~y_off:carbon_y ~z_rot:carbon_z ())

let derek_x = 0.
let derek_y = -2.8
let derek_z = Float.pi /. 25.

let derek_build reset_button =
  build
    ~ports_cutter:
      (Ports.reversible_holder
         ~reset_button
         ~x_off:derek_x
         ~y_off:derek_y
         ~z_rot:derek_z
         () )

let compactyl =
  Model.import "../things/others/dereknheiley_compactyl_5x6.stl"
  |> Model.rotate (0., Float.pi /. -8., 0.)
  |> Model.translate (70., -2., -10.)
  |> Model.color ~alpha:0.25 Color.DarkSlateBlue

let compactyl_compare () =
  Model.union
    [ compactyl
    ; Case.to_scad ~show_caps:false (build ~ports_cutter:(Ports.make ()) ())
      |> Model.color ~alpha:0.5 Color.Yellow
    ]

let carbonfet_ex () =
  let case = carbon_build () in
  Model.union
    [ Case.to_scad ~show_caps:true case
    ; Ports.place_tray
        ~x_off:carbon_x
        ~y_off:carbon_y
        ~z_rot:carbon_z
        case.walls
        (Ports.carbonfet_stl false)
    ]

let reversible_ex ?(reset_button = false) () =
  let case = derek_build reset_button () in
  Model.union
    [ Case.to_scad ~show_caps:true case
    ; Ports.place_tray
        ~x_off:derek_x
        ~y_off:derek_y
        ~z_rot:derek_z
        case.walls
        (Ports.derek_reversible_stl reset_button)
    ]
