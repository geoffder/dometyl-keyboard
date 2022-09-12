open! Scad_ml
open! Generator

let body_lookups =
  let offset = function
    | 2 -> v3 0. 3.5 (-6.) (* middle *)
    | 3 -> v3 0.5 (-2.5) 0.5 (* ring *)
    | i when i >= 4 -> v3 (-1.) (-18.) 8.5 (* pinky *)
    | 0 -> v3 (-2.25) 0. 6.5
    | _ -> v3 0. 0. 0.
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
        ~fan:{ angle = Float.pi /. 9.6; radius = 70.; tilt = Float.pi /. 48. }
        ~well:{ angle = Float.pi /. 7.; radius = 47.; tilt = 0. }
        ())
  in
  Plate.Lookups.thumb ~curve ()

let plate_builder =
  Plate.make
    ~n_body_cols:5
    ~spacing:0.5
    ~tent:(Float.pi /. 16.)
    ~thumb_offset:(v3 (-14.) (-39.) 9.)
    ~thumb_angle:Float.(v3 (pi /. 60.) (pi /. -14.) (pi /. 12.))
    ~rotate_thumb_clips:true
    ~body_lookups
    ~thumb_lookups
    ~caps:Caps.MBK.uniform

let plate_welder plate =
  Scad.union [ Plate.skeleton_bridges plate; Bridge.cols ~columns:plate.body 1 2 ]

let wall_builder plate =
  let eyelet_config = Eyelet.magnet_6x3_config in
  Walls.
    { body =
        auto_body
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
        auto_thumb
          ~south_lookup:(fun _ -> Yes)
          ~east_lookup:(fun _ -> No)
          ~west_lookup:(fun _ -> Eye)
          ~north_clearance:3.
          ~south_clearance:3.
          ~side_clearance:3.
          ~n_steps:(`Flat 4)
          ~eyelet_config
          plate
    }

let base_connector =
  Connect.skeleton
    ~height:9.
    ~index_height:15.
    ~thumb_height:17.
    ~fn:64
    ~fudge_factor:8.
    ~overlap_factor:1.2
    ~close_thumb:true

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

let bottom ?(chonk = false) case =
  (* With 5x1 magnets, for thinner plate. If ~fastener is not specified,
     Bottom.make will default to the same magnet used for the case.
     NOTE: this behaviour does not apply if the case has through-hole eyelets *)
  let fastener =
    if not chonk then Some (Eyelet.Magnet { rad = 2.65; thickness = 1.2 }) else None
  in
  Bottom.make ?fastener case

let tent case = Tent.make ~degrees:30. case
