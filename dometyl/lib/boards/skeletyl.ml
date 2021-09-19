open! Base
open! Scad_ml
open Generator

(* This is configuration is a mimicry of BastardKB's skeletyl keyboard.
   (https://github.com/Bastardkb/Skeletyl).

   This is is a good place to start from if you'd like to have something like the
   skeletyl, but with some more pinky stagger. Though, it will never be as
   pretty. *)
let lookups =
  let offset = function
    | 2 -> 0., 3.5, -8. (* middle *)
    | 3 -> 1., -1., -1.5 (* ring *)
    | i when i >= 4 -> 0., -12., 7. (* pinky *)
    | 0 -> -1., 0., -2.
    | _ -> 0., 0., -3.
  and curve = function
    | 0 ->
      Curvature.(
        curve ~well:(spec ~radius:85. (Float.pi /. 12.) ~tilt:(Float.pi /. 24.)) ())
    | _ -> Curvature.(curve ~well:(spec ~radius:85. (Float.pi /. 12.)) ())
  and swing = function
    | 2 -> Float.pi /. -48.
    | 3 -> Float.pi /. -19.
    | 4 -> Float.pi /. -14.
    | _ -> 0.
  and splay _ = 0.
  and rows _ = 3 in
  Plate.Lookups.make ~offset ~curve ~splay ~swing ~rows ()

let plate_builder =
  Plate.make
    ~n_cols:5
    ~spacing:1.5
    ~tent:(Float.pi /. 10.)
    ~lookups
    ~thumb_offset:(-1., -50., -6.)
    ~thumb_angle:Float.(0., pi /. -4.3, pi /. 6.)
    ~thumb_curve:
      Curvature.(curve ~fan:{ angle = Float.pi /. 12.5; radius = 85.; tilt = 0. } ())

let plate_welder = Plate.skeleton_bridges

let wall_builder plate =
  Walls.
    { body =
        Body.make
          ~n_steps:(`Flat 5)
          ~n_facets:5
          ~north_clearance:1.5
          ~south_clearance:1.5
          ~side_clearance:1.5
          plate
    ; thumb =
        Thumb.make
          ~south_lookup:(fun i -> if not (i = 1) then Yes else No)
          ~east:No
          ~west:Screw
          ~clearance:0.5
          ~d1:4.
          ~d2:4.75
          ~n_steps:(`PerZ 5.)
          ~n_facets:2
          plate
    }

let base_connector =
  Connect.skeleton
    ~height:5.
    ~thumb_height:9.
    ~east_link:(Connect.snake ~scale:1. ~d:1.4 ())
    ~west_link:(Connect.cubic ~scale:1.25 ~d:1. ~bow_out:false ())
    ~cubic_d:2.
    ~cubic_scale:1.
    ~body_join_steps:5
    ~thumb_join_steps:5
    ~fudge_factor:8.
    ~close_thumb:false

let ports_cutter = BastardShield.(cutter (make ()))

let build ?right_hand ?hotswap () =
  let keyhole = Mx.make_hole ~clearance:2.75 ?hotswap () in
  Case.make
    ?right_hand
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    keyhole

let bastard_skelly =
  Model.import "../things/others/bastardkb_skeletyl_v3_v5.stl"
  |> Model.translate (87., 0., 25.)
  |> Model.rotate (Float.pi /. 2., 0., 0.)
  |> Model.translate (0., -2., 8.)
  |> Model.color ~alpha:0.5 Color.DarkSlateBlue

let bastard_compare () =
  Model.union
    [ bastard_skelly
    ; Case.to_scad ~show_caps:false (build ()) |> Model.color ~alpha:0.5 Color.Yellow
    ]

let bk_skeletyl_w_shield () =
  let shield = BastardShield.(make ()) in
  Model.union
    [ bastard_skelly
    ; BastardShield.to_scad ~show_screws:true shield |> Model.translate (-6.71, 35.2, 2.)
    ]
