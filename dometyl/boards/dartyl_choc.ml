(* A choc flavored dactyl with 34 keys *)

open! Scad_ml
open Generator

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
          plate
    ; thumb =
        auto_thumb
          ~north_lookup:(fun _ -> No)
          ~south_lookup:(fun _ -> Yes)
          ~north_clearance:0.5
          ~south_clearance:0.5
          ~side_clearance:0.5 (* ~d1:3. *)
          ~d2:4.75
          ~n_steps:(`Flat 15)
          plate
    }

let base_connector =
  Connect.skeleton
    ~height:6.
    ~thumb_height:9.5
    ~north_joins:(fun i -> i < 2)
    ~close_thumb:true
    ~south_joins:(fun _ -> false)

let body_lookups =
  let offset = function
    | 2 -> v3 (-2.33) 4.6 (-3.) (* middle *)
    | 3 -> v3 (-1.66) (-4.5) 0. (* ring *)
    | i when i >= 4 -> v3 (-3.5) (-19.) 6.5 (* pinky *)
    | 0 -> v3 (-0.85) (-3.5) 13. (* inside *)
    | _ -> v3 (-1.66) (-2.) 5.
  (* index *)
  and curve = function
    | 0 ->
      Curvature.(
        curve ~well:(spec ~radius:42. (Float.pi /. 5.95) ~tilt:(Float.pi /. 5.)) ())
    | _ ->
      Curvature.(
        curve ~well:(spec ~radius:42. (Float.pi /. 6.1) ~tilt:(Float.pi /. 15.)) ())
  and swing = function
    | 2 -> Float.pi /. -48.
    | 3 -> Float.pi /. -23.
    | 4 -> Float.pi /. -20.0
    | _ -> 0.
  and splay = function
    | 4 -> -0.23
    | 3 -> -0.1
    | _ -> 0.
  and rows _ = 3 in
  Plate.Lookups.body ~offset ~curve ~splay ~swing ~rows ()

let thumb_lookups =
  let curve _ =
    Curvature.(
      curve
        ~fan:{ angle = Float.pi /. 10.2; radius = 75.; tilt = Float.pi /. 48. }
        ~well:{ angle = Float.pi /. 7.5; radius = 59.; tilt = 0. }
        ())
  and rows _ = 2 in
  Plate.Lookups.thumb ~curve ~rows ()

let plate_welder = Plate.skeleton_bridges

let ports_cutter =
  Ports.reversible_holder
    ~reset_button:true
    ~x_off:(-1.5)
    ~y_off:(-2.9)
    ~z_rot:0.09
    ~rail_w:1.15
    ()

let build ?hotswap () =
  let keyhole = Choc.make_hole ?hotswap ~outer_h:18. ~thickness:2.8 () in
  let plate_builder =
    Plate.make
      ~n_body_cols:5
      ~spacing:1.5
      ~tent:(Float.pi /. 12.)
      ~body_lookups
      ~thumb_lookups
      ~caps:Caps.MBK.uniform
      ~thumb_caps:Caps.MBK.uniform
      ~thumb_offset:(v3 1. (-48.) 10.)
      ~thumb_angle:Float.(v3 0.23 (-0.3) (pi /. 17.))
  in
  Case.make
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    ~plate_builder
    keyhole
    ~right_hand:true
