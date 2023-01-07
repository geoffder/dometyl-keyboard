(** A lower profile skeletyl, with curvatures adjusted for chocs, and bumpon eyelets built
    in to the case. *)

open OCADml
open OSCADml
open Dometyl

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

(* TODO: Overlapping keyholes leads to failure with the new non-hull way of
    joining the column keys. Should detect if the points of one face lie on the
    wrong side of the others plane, and use Path3.hull instead (to be
    implemented next). *)
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

let wall_builder plate =
  Walls.
    { body =
        auto_body
          ~d1:(`Abs 11.)
          ~d2:9.
          ~n_steps:(`PerZ 1.5)
          ~scale:(v2 0.8 0.9)
          ~scale_ez:(v2 0.42 1., v2 1. 1.)
          ~north_clearance:0.
          ~south_clearance:0.
          ~side_clearance:0.
          plate
    ; thumb =
        auto_thumb
          ~d1:(`Abs 11.)
          ~d2:6.
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
    ~height:7.
    ~index_height:14.
    ~spline_d:1.5
    ~thumb_height:9.
    ~close_thumb:false
    ~corner:(Path3.Round.chamf (`Cut 0.5))
    ~north_joins:(Fun.const true)
    ~south_joins:(fun i -> i = 3)

let ports_cutter = BastardShield.(cutter ~x_off:(-2.) ~z_off:1.5 (make ()))

let build ?right_hand ?hotswap () =
  let keyhole =
    Choc.make_hole ~clearance:0. ~corner:(Path3.Round.chamf (`Cut 0.5)) ?hotswap ()
  and eyelet_config = Eyelet.{ bumpon_config with hole = inset 0.8 }
  and wall_locs =
    Eyelet.(
      Idx.
        [ Body (`W, First)
        ; Body (`N, Idx 2)
        ; Body (`N, Last)
        ; Body (`S, Last)
        ; Body (`S, Idx 2)
        ; Thumb (`N, First)
        ; Thumb (`S, Last)
        ])
  and free_locs =
    [ `U 0.08 ]
    (* NOTE: likely not enough room to cram in one under the index, but
        might be a bit rocky without getting something in there *)
  in
  Case.make
    ?right_hand
    ~eyelets:(Case.eyelets ~config:eyelet_config ~free_locs ~wall_locs ())
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
