open! OCADml
open! OSCADml
open! Generator
open! Syntax

let body_lookups =
  let offset = function
    | 2 -> v3 0. 3.5 (-5.) (* middle *)
    | 3 -> v3 1. (-2.5) 0.5 (* ring *)
    | i when i >= 4 -> v3 0.5 (-18.) 8.5 (* pinky *)
    | 0 -> v3 (-2.5) 0. 5.6 (* inner index *)
    | _ -> v3 0. 0. 0.
  and curve = function
    | i when i = 0 ->
      Curvature.(
        curve ~well:(well ~tilt:(Float.pi /. 7.25) ~radius:41. (Float.pi /. 4.9)) ())
      (* tilted inner index *)
    | i when i = 1 -> Curvature.(curve ~well:(well ~radius:42. (Float.pi /. 4.9)) ())
    | i when i >= 3 ->
      Curvature.(curve ~well:(well ~radius:37.75 (Float.pi /. 4.)) ()) (* ring / pinky *)
    | _ -> Curvature.(curve ~well:(well ~radius:47.25 (Float.pi /. 5.9)) ())
  (* middle *)
  and splay = function
    | i when i = 3 -> Float.pi /. -25. (* ring *)
    | i when i >= 4 -> Float.pi /. -11. (* pinky *)
    | _ -> 0.
  and rows _ = 3 in
  Plate.Lookups.body ~offset ~curve ~splay ~rows ()

let thumb_lookups =
  let curve _ =
    let f = function
      | 2 ->
        Key.quaternion_about_origin (Float.pi /. 30.)
        >> Key.translate (v3 (-0.5) (-0.8) 1.)
      | _ -> Fun.id
    in
    Curvature.(
      post_tweak
        ~fan:(fan ~radius:70. ~tilt:(Float.pi /. 48.) (Float.pi /. 8.8))
        ~well:(well ~radius:47. (Float.pi /. 7.5))
        f)
  in
  Plate.Lookups.thumb ~curve ()

let plate_builder =
  Plate.make
    ~n_body_cols:5
    ~spacing:0.
    ~body_lookups
    ~thumb_lookups
    ~thumb_offset:(v3 (-15.) (-44.) 8.5)
    ~thumb_angle:Float.(v3 (pi /. 60.) (pi /. -14.) (pi /. 12.))
    ~caps:Caps.Matty3.row
    ~thumb_caps:Caps.MT3.thumb_1u

let wall_builder plate =
  Walls.
    { body =
        auto_body
          ~west_lookup:(fun _ -> true)
          ~east_lookup:(fun _ -> true )
            (* ~d1:2. *)
            (* ~d2:5. *)
          ~n_steps:(`Flat 3)
          ~north_clearance:2.5
          ~south_clearance:2.5
          ~side_clearance:1.5
          plate
    ; thumb =
        auto_thumb
          ~south_lookup:(fun _ -> true)
          ~east_lookup:(fun _ -> false)
          ~west_lookup:(fun _ -> true)
          ~n_steps:(`Flat 3)
          ~north_clearance:3.
          ~south_clearance:3.
          ~side_clearance:3.
          plate
    }

let base_connector = Connect.closed
let plate_welder = Plate.column_joins
let ports_cutter = BastardShield.(cutter ~x_off:3. ~y_off:(-1.4) (make ()))

let build ?right_hand ?(empty = false) () =
  (* NOTE: It is strongly advised to use screws with heatset inserts rather than
   magnets when doing a hall-effect rubber dome build to avoid interference. *)
  let eyelets = Case.eyelets ~config:Eyelet.m4_config () in
  (* use empty for quicker preview *)
  let hole = if empty then Niz.make_empty_hole () else Niz.make_hole () in
  Case.make
    ?right_hand
    ~eyelets
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    hole

let fastener = Eyelet.screw_fastener ~clearance:6. () (* countersunk M4 *)
let bottom case = Bottom.make ~fastener case
let tent ?(degrees = 30.) case = Tent.make ~fastener ~degrees case
