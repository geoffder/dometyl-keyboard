open! Base
open! Scad_ml
open! Generator
open! Infix

let lookups =
  let offset = function
    | 2 -> 0., 3.5, -5. (* middle *)
    | 3 -> 1., -2.5, 0.5 (* ring *)
    | i when i >= 4 -> 0.5, -18., 8.5 (* pinky *)
    | 0 -> -2.5, 0., 5.6 (* inner index *)
    | _ -> 0., 0., 0.
  and curve = function
    | i when i = 0 ->
      Curvature.(
        curve ~well:(spec ~tilt:(Float.pi /. 7.25) ~radius:41. (Float.pi /. 4.9)) ())
      (* tilted inner index *)
    | i when i = 1 -> Curvature.(curve ~well:(spec ~radius:42. (Float.pi /. 4.9)) ())
    | i when i >= 3 ->
      Curvature.(curve ~well:(spec ~radius:37.75 (Float.pi /. 4.)) ()) (* ring / pinky *)
    | _ -> Curvature.(curve ~well:(spec ~radius:47.25 (Float.pi /. 5.9)) ())
  (* middle *)
  and splay = function
    | i when i = 3 -> Float.pi /. -25. (* ring *)
    | i when i >= 4 -> Float.pi /. -11. (* pinky *)
    | _ -> 0.
  and rows _ = 3 in
  Plate.Lookups.make ~offset ~curve ~splay ~rows ()

let thumb_curve =
  let f = function
    | 2 ->
      KeyHole.quaternion_about_origin (Float.pi /. 30.)
      >> KeyHole.translate (-0.5, -0.8, 1.)
    | _ -> Fn.id
  in
  Curvature.(
    post_tweak
      ~fan:{ angle = Float.pi /. 8.8; radius = 70.; tilt = Float.pi /. 48. }
      ~well:{ angle = Float.pi /. 7.5; radius = 47.; tilt = 0. }
      f)

let plate_builder =
  Plate.make
    ~n_cols:5
    ~spacing:0.
    ~lookups
    ~thumb_curve
    ~thumb_offset:(-15., -44., 8.5)
    ~thumb_angle:Float.(pi /. 60., pi /. -14., pi /. 12.)
    ~caps:Caps.Matty3.row
    ~thumb_caps:Caps.MT3.thumb_1u

let wall_builder plate =
  let eyelet_config = Eyelet.magnet_6x3_config in
  Walls.
    { body =
        Body.make
          ~west_lookup:(fun i -> if i = 0 then Eye else Yes)
          ~east_lookup:(fun _ -> Yes)
          ~d1:2.
          ~d2:5.
          ~n_facets:2
          ~n_steps:(`PerZ 6.)
          ~north_clearance:2.5
          ~south_clearance:2.5
          ~side_clearance:1.5
          ~eyelet_config
          plate
    ; thumb =
        Thumb.make
          ~south_lookup:(fun _ -> Yes)
          ~east:No
          ~west:Eye
          ~n_facets:3
          ~n_steps:(`Flat 6)
          ~clearance:3.
          ~eyelet_config
          plate
    }

let base_connector =
  Connect.closed
    ~body_steps:(`PerZ 6.)
    ~thumb_steps:(`Flat 6)
    ~overlap_factor:1.5
    ~east_link:(Connect.snake ~height:15. ())
    ~west_link:(Connect.straight ~height:15. ())

let plate_welder = Plate.column_joins
let ports_cutter = BastardShield.(cutter ~x_off:1. ~y_off:(-1.6) (make ()))

let build ?right_hand ?(empty = false) () =
  let hole =
    if empty
    then
      (* mimic the shape of the Niz hole for quicker preview *)
      Mx.make_hole
        ~outer_w:20.5
        ~outer_h:20.5
        ~inner_w:14.
        ~inner_h:14.
        ~thickness:5.6
        ~clearance:4.
        ~cap_height:6.7
        ()
    else Niz.make_hole ()
  in
  Case.make
    ?right_hand
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    hole