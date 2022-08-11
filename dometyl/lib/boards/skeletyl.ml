(** This is configuration is a mimicry of BastardKB's skeletyl keyboard.
    (https://github.com/Bastardkb/Skeletyl).

    This is is a good place to start from if you'd like to have something like the
    skeletyl, but with some more pinky stagger. Though, it will never be as pretty. *)

open! Base
open! Scad_ml
open Generator

let body_lookups =
  let offset = function
    | 2 -> v3 0. 3.5 (-8.) (* middle *)
    | 3 -> v3 1. (-1.) (-1.5) (* ring *)
    | i when i >= 4 -> v3 0. (-12.) 7. (* pinky *)
    | 0 -> v3 (-1.) 0. (-2.)
    | _ -> v3 0. 0. (-3.)
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
  Plate.Lookups.body ~offset ~curve ~splay ~swing ~rows ()

let thumb_lookups =
  let curve _ =
    Curvature.(curve ~fan:{ angle = Float.pi /. 12.5; radius = 85.; tilt = 0. } ())
  in
  Plate.Lookups.thumb ~curve ()

let plate_builder =
  Plate.make
    ~n_body_cols:5
    ~spacing:1.5
    ~tent:(Float.pi /. 10.)
    ~body_lookups
    ~thumb_lookups
    ~thumb_offset:(v3 (-1.) (-50.) (-6.))
    ~thumb_angle:Float.(v3 0. (pi /. -4.3) (pi /. 6.))

let plate_welder = Plate.skeleton_bridges

let wall_builder plate =
  Walls.
    { body =
        auto_body
          ~n_steps:(`Flat 5)
          ~n_facets:5
          ~north_clearance:1.5
          ~south_clearance:1.5
          ~side_clearance:1.5
          plate
    ; thumb =
        auto_thumb
          ~south_lookup:(fun i -> if not (i = 1) then Yes else No)
          ~east_lookup:(fun _ -> No)
          ~west_lookup:(fun _ -> Eye)
          ~north_clearance:0.5
          ~south_clearance:0.5
          ~side_clearance:0.5
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
    ~east_link:(Connect.snake ~height:9. ~scale:1. ~d:1.4 ())
    ~west_link:(Connect.cubic ~height:11. ~scale:1.25 ~d:1. ~bow_out:false ())
    ~cubic_d:2.
    ~cubic_scale:1.
    ~body_join_steps:(`Flat 5)
    ~thumb_join_steps:(`Flat 5)
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
  Scad.import_3d "../things/others/bastardkb_skeletyl_v3_v5.stl"
  |> Scad.translate (v3 87. 0. 25.)
  |> Scad.xrot (Float.pi /. 2.)
  |> Scad.translate (v3 0. (-2.) 8.)
  |> Scad.color ~alpha:0.5 Color.DarkSlateBlue

let bastard_compare () =
  Scad.union
    [ bastard_skelly
    ; Case.to_scad ~show_caps:false (build ()) |> Scad.color ~alpha:0.5 Color.Yellow
    ]

let bk_skeletyl_w_shield () =
  let shield = BastardShield.(make ()) in
  Scad.union
    [ bastard_skelly
    ; BastardShield.to_scad ~show_screws:true shield
      |> Scad.translate (v3 (-6.71) 35.2 2.)
    ]
