open! Scad_ml
open! Generator
open! Syntax

let body_lookups =
  let offset = function
    | 0 -> v3 (-3.5) (-6.) 7. (* outer index *)
    | 2 -> v3 0. 3. (-5.5) (* middle *)
    | 3 -> v3 0. 0. 0. (* ring *)
    | 4 | 5 -> v3 0. (-12.) 9.5 (* pinky *)
    | _ -> v3 0. (-5.) 1.
  and curve = function
    | i when i = 0 ->
      let f = function
        (* | 3 -> *)
        (*   Key.quaternion_about_origin (Float.pi /. -128.) *)
        (*   >> Key.translate (v3 (-0.5) (-1.5) 1.6) *)
        | _ -> Fun.id
      in
      Curvature.(
        post_tweak ~well:(spec ~tilt:(Float.pi /. 6.7) ~radius:44.1 (Float.pi /. 5.4)) f)
    | _ -> Curvature.(curve ~well:(spec ~radius:42. (Float.pi /. 5.2)) ())
  and swing = function
    | _ -> 0.
  and splay = function
    | _ -> 0.
  and rows = function
    | 2 | 3 -> 5
    | _ -> 4
  and centre = function
    | 2 | 3 -> 2.
    | _ -> 1.
  in
  Plate.Lookups.body ~offset ~curve ~swing ~splay ~rows ~centre ()

(* let body_lookups = *)
(*   let offset = function *)
(*     | 0 -> v3 (-3.5) (-6.) 7. (\* outer index *\) *)
(*     | 2 -> v3 0. 3. (-5.5) (\* middle *\) *)
(*     | 3 -> v3 0. 0. 0. (\* ring *\) *)
(*     | 4 -> v3 0. (-12.) 9.5 (\* pinky *\) *)
(*     | 5 -> v3 5.2 (-13.5) 19. (\* outer pinky *\) *)
(*     | _ -> v3 0. (-5.) 1. *)
(*   and curve = function *)
(*     | i when i = 0 -> *)
(*       let f = function *)
(*         | 3 -> *)
(*           Key.quaternion_about_origin (Float.pi /. -128.) *)
(*           >> Key.translate (v3 (-0.5) (-1.5) 1.6) *)
(*         | _ -> Fun.id *)
(*       in *)
(*       Curvature.( *)
(*         post_tweak ~well:(spec ~tilt:(Float.pi /. 6.7) ~radius:44.1 (Float.pi /. 5.4)) f) *)
(*     | i when i = 5 -> *)
(*       let f = function *)
(*         | 0 -> Key.translate (v3 (-1.) 0. 3.) *)
(*         | 3 -> *)
(*           Key.quaternion_about_origin (Float.pi /. 55.) *)
(*           >> Key.translate (v3 (-0.75) (-4.) 4.) *)
(*         | _ -> Fun.id *)
(*       in *)
(*       Curvature.( *)
(*         post_tweak ~well:(spec ~tilt:(Float.pi /. -4.7) ~radius:44. (Float.pi /. 5.4)) f) *)
(*     | _ -> Curvature.(curve ~well:(spec ~radius:42. (Float.pi /. 5.2)) ()) *)
(*   and swing = function *)
(*     | _ -> 0. *)
(*   and splay = function *)
(*     | _ -> 0. *)
(*   and rows = function *)
(*     | 5 -> 3 *)
(*     | _ -> 4 *)
(*   in *)
(*   Plate.Lookups.body ~offset ~curve ~swing ~splay ~rows () *)

(* let thumb_lookups = *)
(*   let curve _ = *)
(*     Curvature.( *)
(*       curve *)
(*         ~well:{ angle = Float.pi /. 8.; radius = 50.; tilt = 0. } *)
(*         ~fan:{ angle = Float.pi /. 28.; radius = 200.; tilt = 0. } *)
(*         ()) *)
(*   in *)
(*   Plate.Lookups.thumb ~curve () *)
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
    ~n_body_cols:6
    ~spacing:1.
    ~tent:0.
    ~thumb_offset:(v3 (-2.) (-50.) 18.)
    ~thumb_angle:Float.(v3 (pi /. 40.) (pi /. -20. *. 0.) (pi /. 24.))
    ~body_lookups
    ~thumb_lookups
    ~caps:Caps.SA.uniform

let wall_builder plate =
  Walls.
    { body =
        auto_body
          ~d1:(`Abs 14.)
          ~d2:10.
          ~n_steps:(`PerZ 1.5)
          ~scale:(v2 0.8 0.9)
          ~scale_ez:(v2 0.42 1., v2 1. 1.)
          ~east_lookup:(fun i -> if i = 2 then No else No)
          ~west_lookup:(fun i -> if i = 0 || i = 2 then Yes else No)
          ~north_clearance:0.
          ~south_clearance:0.
          ~side_clearance:0.
          plate
    ; thumb =
        auto_thumb
          ~d1:(`Abs 14.)
          ~d2:10.
          ~n_steps:(`PerZ 1.5)
          ~scale:(v2 0.8 0.9)
          ~scale_ez:(v2 0.42 1., v2 1. 1.)
          ~east_lookup:(fun _ -> No)
          ~north_lookup:(fun _ -> No)
          ~south_lookup:(fun i -> if i = 0 then Eye else if i = 2 then Yes else No)
          ~north_clearance:0.
          ~south_clearance:0.
          ~side_clearance:0.
          plate
    }

let plate_welder = Plate.column_joins

(* let base_connector = Connect.closed ~spline_d:0.1 ~max_edge_res:1. *)
let base_connector =
  Connect.skeleton
    ~height:11.
    ~index_height:13.
    ~thumb_height:11.
    ~close_thumb:false
    ~north_joins:(fun i -> i < 2)
    ~south_joins:(Fun.const false)
    ~max_edge_res:0.85

let holder_x = 0.
let holder_y = -2.5
let holder_z = Float.pi /. 25.

let holder ?reset_button () =
  Ports.reversible_holder ?reset_button ~x_off:holder_x ~y_off:holder_y ~z_rot:holder_z ()

let build ?right_hand ?hotswap () =
  let keyhole =
    Mx.make_hole ?hotswap ~clearance:(-15.) ~corner:(Path3.Round.chamf (`Cut 0.5)) ()
  in
  Case.make
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter:Ports.blank
    ?right_hand
    keyhole

let compactyl =
  Scad.import3 "../things/others/dereknheiley_compactyl_5x6.stl"
  |> Scad.rotate (v3 0. (Float.pi /. -8.) 0.)
  |> Scad.translate (v3 70. (-2.) (-10.))
  |> Scad.color ~alpha:0.25 Color.DarkSlateBlue

let compactyl_compare () =
  Scad.union
    [ compactyl
    ; Case.to_scad ~show_caps:false (build ()) |> Scad.color ~alpha:0.5 Color.Yellow
    ]

let reversible_ex ?(reset_button = false) () =
  let case = build () in
  Scad.union
    [ Case.to_scad ~show_caps:true case
    ; Ports.place_tray
        ~x_off:holder_x
        ~y_off:holder_y
        ~z_rot:holder_z
        case.walls
        (Ports.derek_reversible_stl reset_button)
    ]
