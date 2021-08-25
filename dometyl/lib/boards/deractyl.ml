open! Base
open! Scad_ml
open! Infix

let wall_builder plate =
  Walls.
    { body = Body.make ~n_steps:(`Flat 3) ~clearance:4. plate
    ; thumb =
        Thumb.make
          ~south_lookup:(fun _ -> Yes)
          ~east:No
          ~west:Screw
          ~clearance:1.5
          ~n_steps:(`Flat 3)
          plate
    }

let base_connector =
  Connect.skeleton
    ~height:7.
    ~thumb_height:11.
    ~snake_scale:1.3
    ~snake_d:1.4
    ~cubic_d:2.
    ~cubic_scale:1.
    ~thumb_cubic_d:1.
    ~thumb_cubic_scale:1.25
    ~join_steps:3
    ~fudge_factor:8.
    ~close_thumb:true
    ~close_pinky:true

let lookups =
  let offset = function
    | 2 -> 0., 3., -5.5 (* middle *)
    | 3 -> 0., 0., 0. (* ring *)
    | 0 -> -2.5, -5., 7.5 (* outer index *)
    | 5 -> 5.5, -14., 21. (* outer pinky *)
    | i when i >= 4 -> 0.5, -12., 9.5 (* pinky *)
    | _ -> 0., -5., 1.
  (* and curve = function
   *   | i when i = 0 ->
   *     Curvature.(
   *       curve ~well:(spec ~tilt:(Float.pi /. 6.75) ~radius:57.5 (Float.pi /. 8.)) ())
   *   | i when i = 5 ->
   *     let f = function
   *       | 3 ->
   *         KeyHole.quaternion_about_origin (Float.pi /. -64.)
   *         >> KeyHole.translate (1., 1.5, -1.5)
   *       | _ -> Fn.id
   *     in
   *     Curvature.(
   *       post_tweak ~well:(spec ~tilt:(Float.pi /. -5.) ~radius:58. (Float.pi /. 8.)) f) *)
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
    (* | 5 -> Float.pi /. -24. *)
    | _ -> 0.
  and splay = function
    (* | 5 -> Float.pi /. -120. *)
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
      ~thumb_offset:(-16., -49.5, 13.5)
      ~thumb_angle:Float.(pi /. 12., pi /. -4.75, pi /. 5.5)
      ~thumb_curve:
        Curvature.(
          place
            ~fan:{ angle = Float.pi /. 9.; radius = 70.; tilt = Float.pi /. 24. }
            ~well:{ angle = Float.pi /. 8.; radius = 50.; tilt = 0. }
            ~centre_idx:1)
      ~lookups
      keyhole
  in
  Case.make ~plate_welder ~wall_builder ~base_connector plate

let bastard_compare () =
  Model.union
    [ Skeletyl.bastard_skelly
    ; Case.to_scad ~show_caps:false (build ()) |> Model.color ~alpha:0.5 Color.Yellow
    ]
