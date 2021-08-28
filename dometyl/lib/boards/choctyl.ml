open! Base
open! Scad_ml

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
    ~join_steps:3
    ~fudge_factor:8.
    ~close_thumb:true
    ~close_pinky:false

let lookups =
  let offset = function
    | 2 -> 0., 4., -6. (* middle *)
    | 3 -> 2.75, -1., 0. (* ring *)
    | i when i >= 4 -> 1.0, -22., 9.5 (* pinky *)
    | 0 -> -2.25, 0., 8.
    | _ -> 0., 0., 1.5
  and curve = function
    | i when i >= 3 ->
      Curvature.(curve ~well:(spec ~radius:35. (Float.pi /. 5.3)) ()) (* pinky and ring *)
    | i when i = 0 ->
      Curvature.(
        curve ~well:(spec ~tilt:(Float.pi /. 5.) ~radius:38. (Float.pi /. 5.3)) ())
    | _ -> Curvature.(curve ~well:(spec ~radius:40. (Float.pi /. 6.0)) ())
  and splay = function
    | i when i = 3 -> Float.pi /. -25. (* ring *)
    | i when i >= 4 -> Float.pi /. -15. (* pinky *)
    | _ -> 0.
  in
  Plate.Lookups.make ~offset ~curve ~splay ()

let plate_welder plate =
  Model.union [ Plate.skeleton_bridges plate; Bridge.cols ~columns:plate.columns 1 2 ]

let build () =
  let keyhole = Choc.make_hole ~cap:Caps.mbk () in
  let plate =
    Plate.make
      ~n_rows:3
      ~n_cols:5
      ~spacing:0.5
      ~tent:(Float.pi /. 12.)
      ~thumb_offset:(-16., -44.5, 13.5)
      ~thumb_angle:Float.(pi /. 12., pi /. -4.75, pi /. 5.5)
      ~thumb_curve:
        Curvature.(
          curve
            ~fan:{ angle = Float.pi /. 10.; radius = 70.; tilt = Float.pi /. 24. }
            ~well:{ angle = Float.pi /. 8.; radius = 50.; tilt = 0. }
            ())
      ~lookups
      keyhole
  in
  Case.make ~plate_welder ~wall_builder ~base_connector plate
