open! Base
open! Scad_ml
open! Generator

let lookups =
  let offset = function
    | 2 -> 0., 3.5, -5. (* middle *)
    | 3 -> 1., -2.5, 0.5 (* ring *)
    | i when i >= 4 -> 0.5, -18., 8.5 (* pinky *)
    | 0 -> -2., 0., 5.5
    | _ -> 0., 0., 0.
  and curve = function
    | i when i >= 3 ->
      Curvature.(curve ~well:(spec ~radius:36. (Float.pi /. 4.25)) ())
      (* ring and pinky *)
    | i when i = 0 ->
      Curvature.(
        curve ~well:(spec ~tilt:(Float.pi /. 7.5) ~radius:45. (Float.pi /. 5.95)) ())
    | _ -> Curvature.(curve ~well:(spec ~radius:45.5 (Float.pi /. 6.1)) ())
  and splay = function
    | i when i = 3 -> Float.pi /. -25. (* ring *)
    | i when i >= 4 -> Float.pi /. -11. (* pinky *)
    | _ -> 0.
  and rows _ = 3 in
  Plate.Lookups.make ~offset ~curve ~splay ~rows ()

let plate_builder =
  Plate.make
    ~n_cols:5
    ~lookups
    ~thumb_curve:
      Curvature.(
        curve
          ~fan:{ angle = Float.pi /. 9.; radius = 70.; tilt = Float.pi /. 48. }
          ~well:{ angle = Float.pi /. 7.5; radius = 47.; tilt = 0. }
          ())
    ~thumb_offset:(-18., -40., 12.)
    ~thumb_angle:Float.(pi /. 30., pi /. -9., pi /. 12.)
    ~caps:Caps.Matty3.row
    ~thumb_caps:Caps.MT3.thumb_1u

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

let base_connector =
  Connect.closed ~n_steps:4 ~west_link:(Connect.straight ~height:15. ())

let plate_welder = Plate.column_joins
let ports_cutter = Ports.make ()

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
