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
      Curvature.(curve ~well:(well ~radius:30.8 (Float.pi /. 4.25)) ()) (* ring  *)
    | i when i > 3 ->
      Curvature.(curve ~well:(well ~radius:26.9 (Float.pi /. 3.55)) ()) (* pinky  *)
    | i when i = 0 ->
      Curvature.(
        curve ~well:(well ~tilt:(Float.pi /. 5.6) ~radius:33.2 (Float.pi /. 4.4)) ())
    | _ -> Curvature.(curve ~well:(well ~radius:35.9 (Float.pi /. 5.18)) ())
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
        ~fan:(fan ~radius:70. ~tilt:(Float.pi /. 48.) (Float.pi /. 9.6))
        ~well:(well ~radius:47. (Float.pi /. 7.))
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
  Walls.
    { body =
        auto_body
          ~n_steps:(`Flat 3)
          ~north_clearance:3.5
          ~south_clearance:3.5
          ~side_clearance:2.5
          ~west_lookup:(fun i -> i < 2)
          plate
    ; thumb =
        auto_thumb
          ~south_lookup:(fun _ -> true)
          ~east_lookup:(fun _ -> false)
          ~west_lookup:(fun _ -> true)
          ~north_clearance:3.
          ~south_clearance:3.
          ~side_clearance:3.
          ~n_steps:(`Flat 4)
          plate
    }

let base_connector =
  Connect.skeleton ~height:9. ~index_height:15. ~thumb_height:17. ~fn:64 ~close_thumb:true

let ports_cutter = BastardShield.(cutter ~x_off:0.5 ~y_off:(-1.2) (make ()))

let build ?right_hand ?hotswap () =
  Case.make
    ?right_hand
    ~eyelet_config:Eyelet.magnet_6x3_config
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
