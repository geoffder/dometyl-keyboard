open! Base
open! Scad_ml
open Generator

let bumpon = { Screw.bumpon_config with outer_rad = (2. /. 2. +. 1.); inner_rad = (2. /. 2.)}

let wall_builder plate =
  Walls.
    { body = Body.make 
        ~n_steps:(`Flat 10) 
        ~north_clearance: 2.19
        ~south_clearance: 2.19
        ~side_clearance: 2.19
        ~thickness: 5.0
        ~screw_config:bumpon 
        plate
    ; thumb =
        Thumb.make
          ~north_lookup:(fun _ -> No)
          ~south_lookup:(fun _ -> Yes)
          (* ~west_lookup:(fun i -> if not (i = 1) then Yes else No) *)
          (* ~west:No *)
          ~west:Screw
          ~clearance:0.5
          ~d1:3.
          ~d2:4.75
          ~n_facets:60
          ~screw_config:bumpon
          ~n_steps:(`Flat 15)
          plate
    }

let base_connector =
  Connect.skeleton
    ~height:4.
    ~thumb_height:9.5
    ~snake_scale:1.25
    ~snake_d:5.
    ~cubic_d:3.
    ~cubic_scale:1.
    ~thumb_cubic_d:2.
    ~thumb_cubic_scale:0.001
    ~thumb_join_steps:14
    ~body_join_steps:10
    ~fudge_factor:10.
    ~north_joins:(fun i -> i < 2)
    ~close_thumb:true
    ~south_joins:(fun _ -> false)

let lookups =
  let offset = function
    | 2 -> -1., 4.6, -3. (* middle *)
    | 3 -> 0.33, 0., 0. (* ring *)
    | i when i >= 4 -> 0.5, -14., 5. (* pinky *)
    | 0 -> 0., -3., 12. (* inside *)
    | _ -> -0.66, 0., 5. (* index *)
  and curve = function
    | 0 ->
      Curvature.(
        curve ~well:(spec ~radius:82. (Float.pi /. 12.) ~tilt:(Float.pi /. 10.)) ())
    | _ -> Curvature.(curve ~well:(spec ~radius:82. (Float.pi /. 12.) ~tilt:(Float.pi /. 15.)) ())
  and swing = function
    | 2 -> Float.pi /. -48.
    | 3 -> Float.pi /. -19.
    | 4 -> Float.pi /. -10.0
    | _ -> 0.
  and splay = function
    | 4 -> -0.13
    | 3 -> -0.05
    | _ -> 0.
  and rows _ = 3 in
  Plate.Lookups.make ~offset ~curve ~splay ~swing ~rows ()

let plate_welder = Plate.skeleton_bridges
(* let ports_cutter = Ports.make *)
let ports_cutter = (Ports.reversible_holder 
  ~reset_button:true 
  ~x_off:2.7 
  ~y_off:(-1.9) 
  ~z_rot:0.09 ())

let build () =
  (* let keyhole = Mx.make_hole ~cap:Caps.SA.r3 ~clearance:0.5 ~thickness:4.88 ~hotswap:`South () in *)
  let keyhole = Mx.make_hole ~cap:Caps.SA.r3 ~clearance:0.5 ~thickness:4.88 () in
  let plate_builder =
    Plate.make
      ~n_cols:5
      ~spacing:1.5
      ~tent:(Float.pi /. 12.)
      ~lookups
      ~n_thumb_keys:2
      ~thumb_offset:(4., -45., 10.)
      ~thumb_angle:Float.(0., -0.133, pi /. 17.)
      ~thumb_curve:
        Curvature.(curve ~fan:{ angle = Float.pi /. 10.5; radius = 75.; tilt = 0.2 } ())
  in
  Case.make ~plate_welder ~wall_builder ~base_connector ~ports_cutter ~plate_builder keyhole ~right_hand:false

