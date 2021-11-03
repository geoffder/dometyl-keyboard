open! Base
open! Scad_ml
open! Generator

(* NOTE: The curvature of this configuration is made with MT3 caps in mind.
   As SA caps are taller, they will likely be in collision with one another, as
   can be seen if you set `Plate.make ~caps:Caps.SA.row`. The inner index Matty3
   key models appear to collide here, but cases printed with this config do make
   room for real MT3 caps. *)
let body_lookups =
  let offset = function
    | 2 -> 0., 3.5, -5. (* middle *)
    | 3 -> 1., -2.5, 0.5 (* ring *)
    | i when i >= 4 -> 0.5, -18., 8.5 (* pinky *)
    | 0 -> -2.5, 0., 5.
    | _ -> 0., 0., 0.
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
    ~thumb_offset:(-13., -41., 10.)
    ~thumb_angle:Float.(pi /. 40., pi /. -14., pi /. 24.)
    ~rotate_thumb_clips:false
    ~caps:Caps.Matty3.row
    ~thumb_caps:Caps.MT3.(fun i -> if i = 1 then space_1_25u else space_1u)

let wall_builder plate =
  let eyelet_config = Eyelet.m4_config in
  Walls.
    { body =
        auto_body
          ~n_steps:(`Flat 3)
          ~north_clearance:2.5
          ~south_clearance:2.5
          ~side_clearance:1.5
          ~eyelet_config
          plate
    ; thumb =
        auto_thumb
          ~south_lookup:(fun _ -> Yes)
          ~east_lookup:(fun _ -> No)
          ~west_lookup:(fun _ -> Eye)
          ~north_clearance:3.
          ~south_clearance:3.
          ~side_clearance:3.
          ~n_steps:(`Flat 3)
          ~eyelet_config
          plate
    }

let base_connector =
  Connect.skeleton
    ~n_facets:1
    ~height:9.
    ~index_height:12.
    ~thumb_height:12.
    ~east_link:(Connect.snake ~height:12. ~scale:1.5 ~d:1.2 ())
    ~west_link:(Connect.straight ~height:12. ())
    ~cubic_d:2.
    ~cubic_scale:1.5
    ~body_join_steps:(`Flat 3)
    ~thumb_join_steps:(`Flat 3)
    ~fudge_factor:8.
    ~close_thumb:true
    ~pinky_elbow:false
    ~overlap_factor:1.

let plate_welder plate =
  Scad.union [ Plate.skeleton_bridges plate; Bridge.cols ~columns:plate.body 1 2 ]

let ports_cutter = BastardShield.(cutter ~x_off:0. ~y_off:(-1.) (make ()))

let build ?right_hand ?hotswap () =
  Case.make
    ?right_hand
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    (Mx.make_hole ?hotswap ~clearance:2. ())

let fastener = Eyelet.m4_countersunk_fastener
let bottom case = Bottom.make ~fastener case
let tent case = Tent.make ~fastener ~degrees:30. case

let bastard_compare () =
  Scad.union
    [ Skeletyl.bastard_skelly
    ; Case.to_scad ~show_caps:false (build ()) |> Scad.color ~alpha:0.5 Color.Yellow
    ]
