open! Base
open! Scad_ml
open! Infix

let wall_builder plate =
  Walls.
    { body =
        Body.make
          ~west_lookup:(fun i -> if i = 1 then Screw else Yes)
          ~east_lookup:(fun i -> if i = 1 then Screw else Yes)
          ~n_steps:(`PerZ 6.)
          ~clearance:4.
          plate
    ; thumb =
        Thumb.make
          ~east:No
          ~north_lookup:(fun _ -> No)
          ~south_lookup:(fun i -> if i = 1 then Screw else Yes)
          ~n_steps:(`PerZ 6.)
          plate
    }

let base_connector =
  Connect.closed ~n_steps:4 ~snake_d:2. ~snake_scale:3. ~snake_height:10. ~snake_steps:6

let lookups =
  let offset = function
    | 0 -> -2.5, -5., 7.5 (* outer index *)
    | 2 -> 0., 3., -5.5 (* middle *)
    | 3 -> 0., 0., 0. (* ring *)
    | 4 -> 0., -12., 9.5 (* pinky *)
    | 5 -> 5., -14., 21. (* outer pinky *)
    | _ -> 0., -5., 1.
  and curve = function
    | i when i = 0 ->
      Curvature.(
        curve ~well:(spec ~tilt:(Float.pi /. 6.75) ~radius:57.5 (Float.pi /. 8.)) ())
    | i when i = 5 ->
      let f = function
        | 3 ->
          KeyHole.quaternion_about_origin (Float.pi /. 128.)
          >> KeyHole.translate (-0.5, -0.5, 1.)
        | _ -> Fn.id
      in
      Curvature.(
        post_tweak ~well:(spec ~tilt:(Float.pi /. -4.) ~radius:58. (Float.pi /. 8.)) f)
    | _ -> Curvature.(curve ~well:(spec ~radius:60. (Float.pi /. 8.)) ())
  and swing = function
    | _ -> 0.
  and splay = function
    | _ -> 0.
  in
  Plate.Lookups.make ~offset ~curve ~swing ~splay ()

(* let plate_welder plate =
 *   Model.union [ Plate.skeleton_bridges plate; Bridge.cols ~columns:plate.columns 1 2 ] *)
let plate_welder = Plate.column_joins

let build () =
  (* let keyhole = Mx.make_hole ~cap:Caps.sa_r3 ~hotswap:`South () in *)
  let keyhole = Mx.make_hole ~cap:Caps.sa_r3 () in
  let plate =
    Plate.make
      ~n_rows:4
      ~n_cols:6
      ~spacing:1.
      ~tent:(Float.pi /. 12.)
      ~thumb_offset:(7., -52., 11.5)
      ~thumb_angle:Float.(0., pi /. -3.8, pi /. 6.5)
      ~thumb_curve:
        Curvature.(
          place
            ~well:{ angle = Float.pi /. 8.; radius = 50.; tilt = 0. }
            ~fan:{ angle = Float.pi /. 28.; radius = 200.; tilt = 0. }
            ~centre_idx:1)
      ~lookups
      keyhole
  in
  Case.make ~plate_welder ~wall_builder ~base_connector plate

let compactyl =
  Model.import "../things/compactyl_5x6.stl"
  |> Model.rotate (0., Float.pi /. -8., 0.)
  |> Model.translate (70., -2., -10.)
  |> Model.color ~alpha:0.25 Color.DarkSlateBlue

let compactyl_compare () =
  Model.union
    [ compactyl
    ; Case.to_scad ~show_caps:false (build ()) |> Model.color ~alpha:0.5 Color.Yellow
    ]
