(** This is configuration is a mimicry of BastardKB's skeletyl keyboard.
    (https://github.com/Bastardkb/Skeletyl).

    This is is a good place to start from if you'd like to have something like the
    skeletyl, but with some more pinky stagger. Though, it will never be as pretty. *)

open OCADml
open OSCADml
open Dometyl

let body_lookups =
  let offset = function
    | 2 -> v3 0. 3.5 (-8.) (* middle *)
    | 3 -> v3 1. (-1.) (-1.5) (* ring *)
    | i when i >= 4 -> v3 0. (-12.) 7.7 (* pinky *)
    | 0 -> v3 (-1.) 0. (-2.)
    | _ -> v3 0. 0. (-3.)
  and curve = function
    | 0 ->
      Curvature.(
        curve ~well:(well ~radius:85. (Float.pi /. 12.) ~tilt:(Float.pi /. 24.)) ())
    | _ -> Curvature.(curve ~well:(well ~radius:85. (Float.pi /. 12.)) ())
  and swing = function
    | 2 -> Float.pi /. -48.
    | 3 -> Float.pi /. -19.
    | 4 -> Float.pi /. -14.
    | _ -> 0.
  and splay _ = 0.
  and rows _ = 3 in
  Plate.Lookups.body ~offset ~curve ~splay ~swing ~rows ()

let thumb_lookups =
  let curve _ = Curvature.(curve ~fan:(fan ~radius:85. (Float.pi /. 12.5)) ()) in
  Plate.Lookups.thumb ~curve ()

let plate_builder =
  Plate.make
    ~n_body_cols:5
    ~spacing:1.5
    ~tent:(Float.pi /. 10.)
    ~body_lookups
    ~thumb_lookups
    ~thumb_offset:(v3 0.25 (-50.) (-1.))
    ~thumb_angle:Float.(v3 0. (pi /. -4.3) (pi /. 6.))

let plate_welder = Plate.skeleton_bridges

let wall_builder plate =
  Walls.
    { body =
        auto_body
          ~d1:(`Abs 10.)
          ~d2:5.
          ~n_steps:(`PerZ 1.25)
          ~scale:(v2 0.8 0.9)
          ~scale_ez:(v2 0.42 1., v2 1. 1.)
          ~north_clearance:0.
          ~south_clearance:0.
          ~side_clearance:0.
          plate
    ; thumb =
        auto_thumb
          ~d1:(`Abs 11.)
          ~d2:5.
          ~n_steps:(`Flat 20)
          ~scale:(v2 0.8 0.9)
          ~scale_ez:(v2 0.42 1., v2 1. 1.)
          ~south_lookup:(fun i -> i <> 1)
          ~east_lookup:(fun _ -> false)
          ~west_lookup:(fun _ -> false)
          ~north_clearance:0.
          ~south_clearance:0.
          ~side_clearance:0.
          plate
    }

let base_connector =
  Connect.skeleton
    ~fn:64
    ~height:9.
    ~spline_d:1.5
    ~index_height:16.
    ~thumb_height:14.
    ~close_thumb:false
    ~corner:(Path3.Round.chamf (`Cut 0.5))
    ~north_joins:(Fun.const true)

let ports_cutter = BastardShield.(cutter ~y_off:0.5 (make ()))

let build ?right_hand ?hotswap () =
  let keyhole =
    Mx.make_hole
      ~cap_cutout_height:None
      ?hotswap
      ~clearance:2.75
      ~corner:(Path3.Round.chamf (`Cut 0.5))
      ()
  and eyelets =
    Case.eyelets ~wall_locs:Eyelet.(Thumb (`S, Idx.Idx 0) :: default_wall_locs) ()
  in
  Case.make
    ?right_hand
    ~eyelets
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    keyhole

let bottom case =
  let bump_locs =
    Bottom.
      [ thumb ~loc:(v2 0. 0.5) Last First
      ; thumb ~loc:(v2 0.7 0.) Last Last
      ; body ~loc:(v2 0.1 0.9) First Last
      ; body ~loc:(v2 0.5 1.) (Idx 3) Last
      ; body ~loc:(v2 0.9 0.6) Last Last
      ; body ~loc:(v2 0.8 0.) Last First
      ]
  in
  Bottom.make ~bump_locs case

let bastard_skelly =
  Util.imports
  |> Printf.sprintf "%s/stls/other_dactyls/bastardkb_skeletyl_v4_103.stl"
  |> Scad.import3
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
