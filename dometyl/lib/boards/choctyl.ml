open! Base
open! Scad_ml
open! Generator

let body_lookups =
  let offset = function
    | 2 -> 0., 3.5, -6. (* middle *)
    | 3 -> 0.5, -2.5, 0.5 (* ring *)
    | i when i >= 4 -> -1., -18., 8.5 (* pinky *)
    | 0 -> -2.25, 0., 6.5
    | _ -> 0., 0., 0.
  and curve = function
    | i when i = 3 ->
      Curvature.(curve ~well:(spec ~radius:30.8 (Float.pi /. 4.25)) ()) (* ring  *)
    | i when i > 3 ->
      Curvature.(curve ~well:(spec ~radius:26.9 (Float.pi /. 3.55)) ()) (* pinky  *)
    | i when i = 0 ->
      Curvature.(
        curve ~well:(spec ~tilt:(Float.pi /. 5.6) ~radius:33.2 (Float.pi /. 4.4)) ())
    | _ -> Curvature.(curve ~well:(spec ~radius:35.9 (Float.pi /. 5.18)) ())
  and splay = function
    | i when i = 3 -> Float.pi /. -25. (* ring *)
    | i when i >= 4 -> Float.pi /. -11. (* pinky *)
    | _ -> 0.
  and rows _ = 3 in
  Plate.Lookups.body ~offset ~curve ~splay ~rows ()

let thumb_lookups =
  let curve _ =
    Curvature.(
      curve
        ~fan:{ angle = Float.pi /. 9.5; radius = 70.; tilt = Float.pi /. 48. }
        ~well:{ angle = Float.pi /. 7.; radius = 47.; tilt = 0. }
        ())
  in
  Plate.Lookups.thumb ~curve ()

let plate_builder =
  Plate.make
    ~n_body_cols:5
    ~spacing:0.5
    ~tent:(Float.pi /. 16.)
    ~thumb_offset:(-12., -39., 14.)
    ~thumb_angle:Float.(pi /. 40., pi /. -14., pi /. 24.)
    ~rotate_thumb_clips:true
    ~body_lookups
    ~thumb_lookups
    ~caps:Caps.MBK.uniform

let plate_welder plate =
  Scad.union [ Plate.skeleton_bridges plate; Bridge.cols ~columns:plate.body 1 2 ]

let wall_builder plate =
  (* 6x2 magnet *)
  let eyelet_config =
    Eyelet.{ outer_rad = 4.; inner_rad = 3.; thickness = 3.; hole = Inset 2. }
  in
  Walls.
    { body =
        make_body
          ~index_thickness:4.
          ~n_facets:3
          ~n_steps:(`Flat 3)
          ~north_clearance:3.5
          ~south_clearance:3.5
          ~side_clearance:2.5
          ~west_lookup:(function
            | 0 -> Eye
            | 1 -> Yes
            | _ -> No )
          ~eyelet_config
          plate
    ; thumb =
        make_thumb
          ~south_lookup:(fun _ -> Yes)
          ~east_lookup:(fun _ -> No)
          ~west_lookup:(fun _ -> Eye)
          ~north_clearance:3.
          ~south_clearance:3.
          ~side_clearance:3.
          ~n_facets:3
          ~n_steps:(`Flat 4)
          ~eyelet_config
          plate
    }

let base_connector =
  Connect.skeleton
    ~height:7.
    ~index_height:15.
    ~thumb_height:17.
    ~east_link:(Connect.snake ~scale:1.3 ~d:1.4 ())
    ~west_link:(Connect.straight ~height:15. ())
    ~cubic_d:2.
    ~cubic_scale:1.
    ~n_steps:9
    ~body_join_steps:(`Flat 3)
    ~thumb_join_steps:(`Flat 4)
    ~fudge_factor:8.
    ~overlap_factor:1.2
    ~close_thumb:true
    ~pinky_elbow:false

let ports_cutter = BastardShield.(cutter ~x_off:0.5 ~y_off:(-1.2) (make ()))

let build ?right_hand ?hotswap () =
  Case.make
    ?right_hand
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    (Choc.make_hole ?hotswap ~outer_h:17. ())
