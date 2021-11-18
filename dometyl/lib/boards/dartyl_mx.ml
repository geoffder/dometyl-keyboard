(* A mx flavored dactyl with 34 keys *)

open! Base
open! Scad_ml
open Generator

let bumpon =
  { Eyelet.bumpon_config with outer_rad = (2. /. 2.) +. 1.; inner_rad = 2. /. 2. }

let wall_builder plate =
  Walls.
    { body =
        auto_body
          ~n_steps:(`Flat 4)
          ~north_clearance:2.19
          ~south_clearance:2.19
          ~side_clearance:2.19
          ~west_lookup:(fun i -> if i = 0 then Yes else No)
          ~north_lookup:(fun _ -> Yes)
          ~south_lookup:(fun i -> if i = 0 then No else Yes)
          ~thickness:5.0
          ~eyelet_config:bumpon
          plate
    ; thumb =
        auto_thumb
          ~north_lookup:(fun _ -> No)
          ~south_lookup:(fun _ -> Yes)
          ~north_clearance:0.5
          ~south_clearance:0.5
          ~side_clearance:0.5
          ~d1:3.
          ~d2:4.75
          ~n_facets:60
          ~eyelet_config:bumpon
          ~n_steps:(`Flat 15)
          plate
    }

let base_connector =
  Connect.skeleton
    ~height:4.
    ~thumb_height:9.5
    ~east_link:(Connect.snake ~height:11. ~scale:1.25 ~d:5. ())
    ~west_link:(Connect.straight ())
    ~index_height:20.
    ~cubic_d:3.
    ~cubic_scale:1.
    ~thumb_join_steps:(`Flat 14)
    ~body_join_steps:(`Flat 10)
    ~fudge_factor:10.
    ~north_joins:(fun i -> i < 2)
    ~close_thumb:true
    ~south_joins:(fun _ -> false)

let body_lookups =
  let offset = function
    | 2 -> -1., 4.6, -3. (* middle *)
    | 3 -> 0.33, -2., 0. (* ring *)
    | i when i >= 4 -> 0.5, -19., 6.5 (* pinky *)
    | 0 -> -3., -3.5, 14. (* inside *)
    | _ -> -1.66, -3., 5. (* index *)
  and curve = function
    | 0 ->
      Curvature.(
        curve ~well:(spec ~radius:48.5 (Float.pi /. 5.95) ~tilt:(Float.pi /. 5.)) ())
    | _ ->
      Curvature.(
        curve ~well:(spec ~radius:48. (Float.pi /. 6.1) ~tilt:(Float.pi /. 15.)) ())
  and swing = function
    | 2 -> Float.pi /. -48.
    | 3 -> Float.pi /. -23.
    | 4 -> Float.pi /. -20.0
    | _ -> 0.
  and splay = function
    | 4 -> -0.13
    | 3 -> -0.05
    | _ -> 0.
  and rows _ = 3 in
  Plate.Lookups.body ~offset ~curve ~splay ~swing ~rows ()

let thumb_lookups =
  let curve _ =
    Curvature.(curve 
      ~fan:{ angle = Float.pi /. 11.0; radius = 87.; tilt = Float.pi /. 48. } 
      ~well:{ angle = Float.pi /. 7.5; radius = 49.; tilt = 0. } ())
  and rows _ = 2 in
  Plate.Lookups.thumb ~curve ~rows ()


let plate_welder = Plate.skeleton_bridges

let ports_cutter =
  Ports.reversible_holder ~reset_button:true ~x_off:1.8 ~y_off:(-2.2) ~z_rot:0.09 ~rail_w:1.5 ()

let build ?hotswap () =
  let keyhole = Mx.make_hole ~clearance:1.5 ~thickness:4.88 ?hotswap () in

  let plate_builder =
    Plate.make
      ~n_body_cols:5
      ~spacing:1.5
      ~tent:(Float.pi /. 12.)
      ~body_lookups
      ~thumb_lookups
      ~caps:Caps.Matty3.row
      ~thumb_caps:Caps.MT3.thumb_1u
      ~thumb_offset:(-1., -53., 10.)
      ~thumb_angle:Float.(0., -0.3, pi /. 17.)
  in
  Case.make
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    ~plate_builder
    keyhole
    ~right_hand:true
