open! Scad_ml
open! Generator

(* NOTE: The curvature of this configuration is made with MT3 caps in mind.
   As SA caps are taller, they will likely be in collision with one another, as
   can be seen if you set `Plate.make ~caps:Caps.SA.row`. The inner index Matty3
   key models appear to collide here, but cases printed with this config do make
   room for real MT3 caps. *)
let body_lookups =
  let offset = function
    | 2 -> v3 0. 3.5 (-5.) (* middle *)
    | 3 -> v3 1. (-2.5) 0.5 (* ring *)
    | i when i >= 4 -> v3 0.5 (-18.) 8.5 (* pinky *)
    | 0 -> v3 (-2.5) 0. 5.
    | _ -> v3 0. 0. 0.
  and curve = function
    | i when i >= 3 ->
      Curvature.(curve ~well:(spec ~radius:37. (Float.pi /. 4.25)) ())
      (* ring and pinky *)
    | i when i = 0 ->
      Curvature.(
        curve ~well:(spec ~tilt:(Float.pi /. 7.5) ~radius:46. (Float.pi /. 5.95)) ())
    | _ -> Curvature.(curve ~well:(spec ~radius:46.5 (Float.pi /. 6.1)) ())
  and splay = function
    | i when i = 3 -> Float.pi /. -25. (* ring *)
    | i when i >= 4 -> Float.pi /. -11. (* pinky *)
    | _ -> 0.
  and rows _ = 3 in
  Plate.Lookups.body ~offset ~curve ~splay ~rows ()

let thumb_lookups =
  let curve _ =
    Curvature.(
      curve
        ~fan:{ angle = Float.pi /. 9.; radius = 70.; tilt = Float.pi /. 48. }
        ~well:{ angle = Float.pi /. 7.5; radius = 47.; tilt = 0. }
        ())
  in
  Plate.Lookups.thumb ~curve ()

let plate_builder =
  Plate.make
    ~n_body_cols:5
    ~body_lookups
    ~thumb_lookups
    ~thumb_offset:(v3 (-13.) (-41.) 10.)
    ~thumb_angle:Float.(v3 (pi /. 40.) (pi /. -14.) (pi /. 24.))
    ~rotate_thumb_clips:false
    ~caps:Caps.Matty3.row
    ~thumb_caps:Caps.MT3.(fun i -> if i = 1 then space_1_25u else space_1u)

let wall_builder plate =
  let eyelet_config = Eyelet.magnet_6x3_config in
  Walls.
    { body =
        auto_body
          ~d1:(`Abs 14.) (* ~d1:(`Rel 0.4) *)
          ~d2:10. (* ~n_steps:(`Flat 10) *)
          ~n_steps:(`PerZ 2.)
          ~scale:(v2 0.8 0.9) (* ~scale_ez:(v2 0.42 0., v2 1. 1.) *)
          ~scale_ez:(v2 0.42 1., v2 1. 1.)
          ~north_clearance:0.
          ~south_clearance:0.
          ~side_clearance:0.
          ~eyelet_config
          plate
    ; thumb =
        auto_thumb
        (* ~south_lookup:(fun _ -> Yes) *)
        (* ~d1:12. *)
          ~d1:(`Abs 14.)
          ~d2:8.
          ~n_steps:(`PerZ 2.)
          ~north_lookup:(fun _ -> No)
          ~south_lookup:(fun i -> if i = 1 then No else Yes)
          ~east_lookup:(fun _ -> No)
          ~west_lookup:(fun _ -> Eye)
          ~scale:(v2 0.8 0.9) (* ~scale_ez:(v2 0.42 0., v2 1. 1.) *)
          ~scale_ez:(v2 0.42 1., v2 1. 1.)
          ~north_clearance:0.
          ~south_clearance:0.
          ~side_clearance:0.
          ~eyelet_config
          plate
    }

let base_connector =
  Connect.skeleton
    ~height:9.
    ~index_height:12.
    ~thumb_height:12.
    ~east_link:(Connect.snake ~height:12. ~scale:1.5 ~d:1.2 ())
    ~west_link:(Connect.straight ~height:12. ())
    ~cubic_d:2.
    ~cubic_scale:1.5
    ~body_join_steps:(`Flat 3)
    ~thumb_join_steps:(`Flat 3)
    ~fudge_factor:8. (* ~close_thumb:true *)
    ~close_thumb:false
    ~pinky_elbow:false
    ~overlap_factor:1.

let plate_welder plate =
  Scad.union [ Plate.skeleton_bridges plate; Bridge.cols ~columns:plate.body 1 2 ]

let ports_cutter = BastardShield.(cutter ~x_off:0. ~y_off:0. (make ()))

let build ?right_hand ?hotswap () =
  Case.make
    ?right_hand
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    (Mx.make_hole
       ~cap_cutout_height:None
       ?hotswap
       ~clearance:2.
       ~corner:(Path3.Round.bez (`Cut 0.5))
       ~fn:6
       () )
(* (Mx.make_hole ?hotswap ~clearance:2. ~corner:(Path3.Round.chamf (`Cut 0.5)) ()) *)

let bottom ?(chonk = false) case =
  (* With 5x1 magnets, for thinner plate. If ~fastener is not specified,
     Bottom.make will default to the same magnet used for the case.
     NOTE: this behaviour does not apply if the case has through-hole eyelets *)
  let fastener =
    if not chonk then Some (Eyelet.Magnet { rad = 2.65; thickness = 1.2 }) else None
  in
  Bottom.make ?fastener case

let tent case = Tent.make ~degrees:30. case

let bastard_compare () =
  Scad.union
    [ Skeletyl.bastard_skelly
    ; Case.to_scad ~show_caps:false (build ()) |> Scad.color ~alpha:0.5 Color.Yellow
    ]
