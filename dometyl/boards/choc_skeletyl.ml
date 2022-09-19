(** A lower profile skeletyl, with curvatures adjusted for chocs, and bumpon eyelets built
    in to the case. *)

open! Scad_ml
open Generator

let body_lookups =
  let offset = function
    | 2 -> v3 0. 3.5 (-8.) (* middle *)
    | 3 -> v3 0. (-1.) (-1.5) (* ring *)
    | i when i >= 4 -> v3 0. (-12.) 7. (* pinky *)
    | 0 -> v3 (-0.75) 0. 0.
    | _ -> v3 0. 0. (-3.)
  and curve = function
    | 0 ->
      Curvature.(
        curve ~well:(well ~radius:57. (Float.pi /. 9.) ~tilt:(Float.pi /. 11.)) ())
    | _ -> Curvature.(curve ~well:(well ~radius:56.5 (Float.pi /. 9.)) ())
  and swing = function
    | 2 -> Float.pi /. -48.
    | 3 -> Float.pi /. -19.
    | 4 -> Float.pi /. -14.
    | _ -> 0.
  and splay _ = 0.
  and rows _ = 3 in
  Plate.Lookups.body ~offset ~curve ~splay ~swing ~rows ()

let thumb_lookups =
  let curve _ = Curvature.(curve ~fan:(fan ~radius:85. (Float.pi /. 13.8)) ()) in
  Plate.Lookups.thumb ~curve ()

let plate_builder =
  Plate.make
    ~n_body_cols:5
    ~spacing:0.5
    ~tent:(Float.pi /. 10.)
    ~body_lookups
    ~thumb_lookups
    ~thumb_offset:(v3 (-1.) (-49.) (-6.))
    ~thumb_angle:Float.(v3 0. (pi /. -4.3) (pi /. 6.))
    ~rotate_thumb_clips:true
    ~caps:Caps.MBK.uniform

let plate_welder = Plate.skeleton_bridges
let eyelet_config = Eyelet.{ bumpon_config with hole = Inset 0.8 }

let wall_builder plate =
  Walls.
    { body =
        auto_body
          ~n_steps:(`Flat 3)
          ~north_lookup:(fun i -> if i mod 2 = 0 then Eye else Yes)
          ~north_clearance:1.5
          ~south_clearance:1.5
          ~side_clearance:1.5
          ~eyelet_config
          plate
    ; thumb =
        auto_thumb
          ~south_lookup:(fun i -> if i = 0 then Yes else if i = 2 then Eye else No)
          ~east_lookup:(fun _ -> No)
          ~west_lookup:(fun _ -> Eye)
          ~north_clearance:0.5
          ~south_clearance:0.5
          ~side_clearance:0.5 (* ~d1:4. *)
          ~d2:4.75
          ~n_steps:(`PerZ 5.)
          ~eyelet_config
          plate
    }

let base_connector =
  Connect.skeleton ~height:5. ~index_height:14. ~thumb_height:9. ~close_thumb:false

let ports_cutter = BastardShield.(cutter ~x_off:(-2.) ~y_off:(-1.5) ~z_off:4. (make ()))

let build ?right_hand ?hotswap () =
  let keyhole = Choc.make_hole ~clearance:0. ?hotswap () in
  Case.make
    ?right_hand
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    keyhole

let bastard_compare () =
  Scad.union
    [ Skeletyl.bastard_skelly
    ; Case.to_scad ~show_caps:false (build ()) |> Scad.color ~alpha:0.5 Color.Yellow
    ]
