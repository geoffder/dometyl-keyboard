open! Base
open! Scad_ml
open! Generator
open! Infix

let body_lookups =
  let offset = function
    | 0 -> v3 (-3.5) (-6.) 7. (* outer index *)
    | 2 -> v3 0. 3. (-5.5) (* middle *)
    | 3 -> v3 0. 0. 0. (* ring *)
    | 4 -> v3 0. (-12.) 9.5 (* pinky *)
    | 5 -> v3 5.2 (-13.5) 19. (* outer pinky *)
    | _ -> v3 0. (-5.) 1.
  and curve = function
    | i when i = 0 ->
      let f = function
        | 3 ->
          KeyHole.quaternion_about_origin (Float.pi /. -128.)
          >> KeyHole.translate (v3 (-0.5) (-1.5) 1.6)
        | _ -> Fn.id
      in
      Curvature.(
        post_tweak ~well:(spec ~tilt:(Float.pi /. 6.7) ~radius:44.1 (Float.pi /. 5.4)) f)
    | i when i = 5 ->
      let f = function
        | 0 -> KeyHole.translate (v3 (-1.) 0. 3.)
        | 3 ->
          KeyHole.quaternion_about_origin (Float.pi /. 55.)
          >> KeyHole.translate (v3 (-0.75) (-4.) 4.)
        | _ -> Fn.id
      in
      Curvature.(
        post_tweak ~well:(spec ~tilt:(Float.pi /. -4.7) ~radius:44. (Float.pi /. 5.4)) f)
    | _ -> Curvature.(curve ~well:(spec ~radius:42. (Float.pi /. 5.2)) ())
  and swing = function
    | _ -> 0.
  and splay = function
    | _ -> 0.
  and rows = function
    | 5 -> 3
    | _ -> 4
  in
  Plate.Lookups.body ~offset ~curve ~swing ~splay ~rows ()

let thumb_lookups =
  let curve _ =
    Curvature.(
      curve
        ~well:{ angle = Float.pi /. 8.; radius = 50.; tilt = 0. }
        ~fan:{ angle = Float.pi /. 28.; radius = 200.; tilt = 0. }
        ())
  in
  Plate.Lookups.thumb ~curve ()

let plate_builder =
  Plate.make
    ~n_body_cols:6
    ~spacing:1.
    ~tent:(Float.pi /. 12.)
    ~thumb_offset:(v3 7. (-52.) 11.5)
    ~thumb_angle:Float.(v3 0. (pi /. -3.8) (pi /. 6.5))
    ~body_lookups
    ~thumb_lookups
    ~caps:Caps.SA.uniform

let wall_builder plate =
  Walls.
    { body =
        auto_body
          ~west_lookup:(fun i -> if i = 1 then Eye else Yes)
          ~east_lookup:(fun i -> if i = 1 then Eye else Yes)
          ~n_steps:(`Flat 5)
          ~n_facets:2
          ~north_clearance:7.
          ~side_clearance:2.
          ~index_thickness:5.
          plate
    ; thumb =
        auto_thumb
          ~east_lookup:(fun _ -> No)
          ~north_lookup:(fun _ -> No)
          ~south_lookup:(fun i -> if i = 1 then Eye else Yes)
          ~n_steps:(`Flat 5)
          ~n_facets:4
          plate
    }

let plate_welder = Plate.column_joins

let base_connector =
  Connect.closed
    ~body_steps:(`Flat 5)
    ~thumb_steps:(`Flat 5)
    ~east_link:(Connect.snake ~d:2. ~scale:3. ~height:10. ~n_steps:12 ())
    ~west_link:(Connect.cubic ~d:3. ~scale:0.25 ~height:15. ~bow_out:false ())

let holder_x = 0.
let holder_y = -2.5
let holder_z = Float.pi /. 25.

let holder ?reset_button () =
  Ports.reversible_holder ?reset_button ~x_off:holder_x ~y_off:holder_y ~z_rot:holder_z ()

let build ?reset_button ?right_hand ?hotswap () =
  let keyhole = Mx.make_hole ?hotswap () in
  Case.make
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter:(holder ?reset_button ())
    ?right_hand
    keyhole

let compactyl =
  Scad.import_3d "../things/others/dereknheiley_compactyl_5x6.stl"
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
