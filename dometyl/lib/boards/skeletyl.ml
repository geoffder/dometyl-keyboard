open! Base
open! Scad_ml

(* This is configuration is a mimicry of BastardKB's skeletyl keyboard.
   (https://github.com/Bastardkb/Skeletyl).

   This is is a good place to start from if you'd like to have something like the
   skeletyl, but with some more pinky stagger. Though, it will never be as
   pretty. *)
let wall_builder plate =
  Walls.
    { body = Body.make ~n_steps:(`Flat 3) ~clearance:1.5 plate
    ; thumb =
        Thumb.make
          ~south_lookup:(fun _ -> Yes)
          ~east:No
          ~west:Screw
          ~clearance:1.5
          ~n_steps:(`Flat 3)
          plate
    }

let base_connector =
  Connect.skeleton
    ~height:7.
    ~thumb_height:11.
    ~snake_scale:1.3
    ~snake_d:1.4
    ~cubic_d:2.
    ~cubic_scale:1.
    ~thumb_cubic_d:1.
    ~thumb_cubic_scale:1.25
    ~join_steps:3
    ~fudge_factor:8.
    ~close_thumb:true
    ~close_pinky:false

let lookups =
  let offset = function
    | 2 -> 0., 3.5, -8. (* middle *)
    | 3 -> 0., -1., -1.5 (* ring *)
    | i when i >= 4 -> 0., -12., 7. (* pinky *)
    | _ -> 0., 0., -3.
  and well _ = Curvature.{ angle = Float.pi /. 12.; radius = 85.; tilt = 0. }
  and splay _ = 0. in
  Plate.Lookups.make ~offset ~well ~splay ()

let plate_welder = Plate.skeleton_bridges

let build () =
  (* let keyhole = Mx.make_hole ~cap:Caps.sa_r3 ~hotswap:`South () in *)
  let keyhole = Mx.make_hole ~cap:Caps.sa_r3 ~clearance:2.75 () in
  let plate =
    Plate.make
      ~n_rows:3
      ~n_cols:5
      ~spacing:1.5
      ~tent:(Float.pi /. 10.)
      ~lookups
      ~thumb_offset:(-1., -50., -8.)
      ~thumb_angle:Float.(0., pi /. -4.3, pi /. 6.)
      ~thumb_fan:Curvature.{ angle = Float.pi /. 12.5; radius = 85.; tilt = 0. }
      ~thumb_well:Curvature.{ angle = 0.; radius = 0.; tilt = 0. }
      keyhole
  in
  Case.make ~plate_welder ~wall_builder ~base_connector plate

let bastard_skelly =
  Model.import "../things/bk_skeletyl_v3_v5.stl"
  |> Model.translate (87., 0., 25.)
  |> Model.rotate (Float.pi /. 2., 0., 0.)
  |> Model.translate (0., -2., 8.)
  |> Model.color ~alpha:0.5 Color.DarkSlateBlue

let bastard_compare () =
  Model.union
    [ bastard_skelly
    ; Case.to_scad ~show_caps:false (build ()) |> Model.color ~alpha:0.5 Color.Yellow
    ]
