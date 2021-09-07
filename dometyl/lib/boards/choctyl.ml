open! Base
open! Scad_ml
open! Generator

let lookups =
  let offset = function
    | 2 -> 0., 4., -6. (* middle *)
    | 3 -> 1.5, -1., 0. (* ring *)
    | i when i >= 4 -> 0., -22., 9.5 (* pinky *)
    | 0 -> -2.25, 0., 8.
    | _ -> 0., 0., 1.5
  and curve = function
    | i when i = 3 ->
      Curvature.(curve ~well:(spec ~radius:28.2 (Float.pi /. 4.25)) ()) (* ring  *)
    | i when i > 3 ->
      Curvature.(curve ~well:(spec ~radius:24.3 (Float.pi /. 3.55)) ()) (* pinky  *)
    | i when i = 0 ->
      Curvature.(
        curve ~well:(spec ~tilt:(Float.pi /. 5.) ~radius:31. (Float.pi /. 4.4)) ())
    | _ -> Curvature.(curve ~well:(spec ~radius:33.3 (Float.pi /. 5.18)) ())
  and splay = function
    | i when i = 3 -> Float.pi /. -25. (* ring *)
    | i when i >= 4 -> Float.pi /. -13. (* pinky *)
    | _ -> 0.
  in
  Plate.Lookups.make ~offset ~curve ~splay ()

let plate_builder =
  Plate.make
    ~n_rows:3
    ~n_cols:5
    ~spacing:0.5
    ~tent:(Float.pi /. 12.)
    ~thumb_offset:(-18., -42.5, 13.5)
    ~thumb_angle:Float.(pi /. 12., pi /. -4.75, pi /. 5.5)
    ~thumb_curve:
      Curvature.(
        curve
          ~fan:{ angle = Float.pi /. 10.2; radius = 70.; tilt = Float.pi /. 24. }
          ~well:{ angle = Float.pi /. 5.; radius = 30.; tilt = 0. }
          ())
    ~lookups

let plate_welder plate =
  Model.union [ Plate.skeleton_bridges plate; Bridge.cols ~columns:plate.columns 1 2 ]

let wall_builder plate =
  Walls.
    { body =
        Body.make
          ~n_facets:3
          ~n_steps:(`Flat 3)
          ~clearance:2.5
          ~west_lookup:(function
            | 0 -> Screw
            | 1 -> Yes
            | _ -> No )
          plate
    ; thumb =
        Thumb.make
          ~south_lookup:(fun i -> if i = 1 then No else Yes)
          ~east:No
          ~west:Screw
          ~clearance:1.5
          ~n_facets:3
          ~n_steps:(`Flat 4)
          plate
    }

let base_connector =
  Connect.skeleton
    ~height:7.
    ~index_height:15.
    ~thumb_height:17.
    ~snake_scale:1.3
    ~snake_d:1.4
    ~cubic_d:2.
    ~cubic_scale:1.
    ~thumb_cubic_d:0.5
    ~thumb_cubic_scale:1.5
    ~n_steps:9
    ~body_join_steps:3
    ~thumb_join_steps:4
    ~fudge_factor:8.
    ~overlap_factor:1.2
    ~close_thumb:false

(* let ports_cutter = Ports.carbonfet_holder ~x_off:0. ~y_off:(-0.75) ()*)
let ports_cutter = BastardShield.(cutter ~x_off:1. ~y_off:(-1.) (make ()))

let build ?right_hand ?hotswap () =
  Case.make
    ?right_hand
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    (Choc.make_hole ~cap:Caps.mbk ?hotswap ~outer_h:17. ())
