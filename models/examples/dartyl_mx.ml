(* A mx flavored dactyl with 34 keys *)

open! OCADml
open! OSCADml
open Dometyl

let wall_builder plate =
  Walls.
    { body =
        auto_body
          ~n_steps:(`PerZ 1.)
          ~d2:8.
          ~north_clearance:2.19
          ~south_clearance:2.19
          ~side_clearance:2.19
          ~west_lookup:(fun i -> i = 0)
          ~north_lookup:(fun _ -> true)
          ~south_lookup:(fun i -> i <> 0)
          ~scale:(v2 0.8 0.9)
          ~scale_ez:(v2 0.42 1., v2 1. 1.)
          plate
    ; thumb =
        auto_thumb
          ~north_lookup:(fun _ -> false)
          ~south_lookup:(fun _ -> true)
          ~north_clearance:0.5
          ~south_clearance:0.5
          ~side_clearance:0.5
          ~d2:6.
          ~n_steps:(`Flat 15)
          ~scale:(v2 0.7 0.9)
          ~scale_ez:(v2 0.42 1., v2 1. 1.)
          plate
    }

let base_connector =
  Connect.skeleton
    ~height:6.
    ~thumb_height:9.5
    ~north_joins:(fun i -> i < 2)
    ~close_thumb:true
    ~south_joins:(fun _ -> false)
    ~corner:(Path3.Round.chamf (`Cut 0.5))

let body_lookups =
  let offset = function
    | 2 -> v3 (-1.) 4.6 (-3.) (* middle *)
    | 3 -> v3 0.33 (-2.) 0. (* ring *)
    | i when i >= 4 -> v3 0.5 (-19.) 6.5 (* pinky *)
    | 0 -> v3 (-3.) (-3.5) 14. (* inside *)
    | _ -> v3 (-1.66) (-3.) 5.
  (* index *)
  and curve = function
    | 0 ->
      Curvature.(
        curve ~well:(well ~radius:48.5 (Float.pi /. 5.95) ~tilt:(Float.pi /. 5.)) ())
    | _ -> Curvature.(curve ~well:(well ~radius:48. (Float.pi /. 6.1)) ())
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
    Curvature.(
      curve
        ~fan:(fan ~radius:87. ~tilt:(Float.pi /. 48.) (Float.pi /. 11.0))
        ~well:(well ~radius:59. (Float.pi /. 7.5))
        ())
  and rows _ = 2 in
  Plate.Lookups.thumb ~curve ~rows ()

let plate_welder = Plate.skeleton_bridges

let ports_cutter =
  Ports.reversible_holder
    ~reset_button:true
    ~x_off:1.8
    ~y_off:(-2.2)
    ~z_rot:0.09
    ~rail_w:1.5
    ()

let bumpon =
  { Eyelet.bumpon_config with outer_rad = (2. /. 2.) +. 1.; inner_rad = 2. /. 2. }

let build ?hotswap () =
  let keyhole =
    Mx.make_hole
      ~clearance:1.5
      ~thickness:4.88
      ?hotswap
      ~corner:(Path3.Round.chamf (`Cut 0.5))
      ()
  in
  let plate_builder =
    Plate.make
      ~n_body_cols:5
      ~spacing:1.5
      ~tent:(Float.pi /. 12.)
      ~body_lookups
      ~thumb_lookups
      ~caps:Caps.Matty3.row
      ~thumb_caps:Caps.MT3.thumb_1u
      ~thumb_offset:(v3 (-1.) (-53.) 10.)
      ~thumb_angle:Float.(v3 0. (-0.3) (pi /. 17.))
  in
  Case.make
    ~eyelets:(Case.eyelets ~config:bumpon ())
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    ~plate_builder
    keyhole
    ~right_hand:true
