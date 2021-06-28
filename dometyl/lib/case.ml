open! Base
open! Scad_ml
open! Infix

let keyhole = KeyHole.make Niz.hole_config
let n_rows = 3
let centre_idx = 1

let column =
  Column.make
    ~n_keys:n_rows
    ~curve:Curvature.(place ~well:{ angle = Float.pi /. 12.; radius = 85. } ~centre_idx)
    keyhole

let well_column spec =
  Column.make ~n_keys:n_rows ~curve:Curvature.(place ~well:spec ~centre_idx) keyhole

let thumb =
  Column.(
    make
      ~join_ax:`EW
      ~n_keys:3
      ~curve:
        Curvature.(
          place
            ~well:{ angle = Float.pi /. 12.; radius = 85. }
            ~fan:{ angle = Float.pi /. 12.; radius = 85. }
            ~centre_idx:1)
      (KeyHole.rotate (0., 0., Float.pi /. 2.) keyhole)
    (* orient along x-axis *)
    |> rotate (0., 0., Float.pi /. -2.))

module Plate = struct
  type 'k t =
    { scad : Model.t
    ; columns : 'k Column.t Map.M(Int).t
    ; thumb : 'k Column.t
    }

  let n_cols = 5
  let spacing = 2.
  let centre_col = 2
  let tent = Float.pi /. 12.
  let thumb_offset = 7., -50., -3.
  let thumb_angle = Float.(0., pi /. -4., pi /. 5.)
  let clearance = 5.

  (* TODO: tune *)
  let offset_lookup = function
    | 2 -> 0., 6., -6. (* middle *)
    | 3 -> 0., 3., -2. (* ring *)
    (* | i when i >= 4 -> 0., -12., 6. (\* pinky *\) *)
    | i when i >= 4 -> 3., -12., 6. (* pinky *)
    | _ -> 0., 0., 0.

  let well_lookup = function
    (* | i when i >= 4 -> Curvature.{ angle = Float.pi /. 9.; radius = 60. } (\* pinky *\) *)
    | _ -> Curvature.{ angle = Float.pi /. 9.; radius = 60. }
  (* | _ -> Curvature.{ angle = Float.pi /. 12.; radius = 85. } *)

  let splay_lookup = function
    | i when i >= 4 -> Float.pi /. -20. (* pinky *)
    | _ -> 0.

  let col_offsets =
    let space = keyhole.config.outer_w +. spacing in
    let f m i =
      let data = Vec3.(offset_lookup i <+> (space *. Float.of_int i, 0., 0.)) in
      Map.add_exn ~key:i ~data m
    in
    List.fold ~f ~init:(Map.empty (module Int)) (List.range 0 n_cols)

  let centre_offset = Map.find_exn col_offsets centre_col

  let apply_tent off col =
    Column.(rotate_about_pt (0., tent, 0.) Vec3.(off <-> centre_offset) col)

  let place_col ~key:i ~data:off =
    apply_tent off (well_column @@ well_lookup i)
    |> Column.rotate (0., 0., splay_lookup i)
    |> Column.translate off

  let columns, thumb =
    let placed_cols = Map.mapi ~f:place_col col_offsets in
    let lift =
      let lowest_z =
        let face_low ({ points = ps; _ } : KeyHole.Face.t) =
          Points.fold ~f:(fun m p -> Float.min m (Vec3.get_z p)) ~init:Float.max_value ps
        in
        let key_low ({ faces = fs; _ } : _ KeyHole.t) =
          KeyHole.Faces.fold
            ~f:(fun m face -> Float.min m (face_low face))
            ~init:Float.max_value
            fs
        in
        let col_low ({ keys = ks; _ } : _ Column.t) =
          Map.fold
            ~f:(fun ~key:_ ~data m -> Float.min m (key_low data))
            ~init:Float.max_value
            ks
        in
        Map.fold
          ~f:(fun ~key:_ ~data m -> Float.min m (col_low data))
          ~init:Float.max_value
          placed_cols
      in
      clearance -. lowest_z
    in
    let thumb =
      let placed = Column.(rotate thumb_angle thumb |> translate thumb_offset) in
      apply_tent (Map.find_exn placed.keys 1).origin placed
      |> Column.translate (0., 0., lift)
    in
    Map.map ~f:(Column.translate (0., 0., lift)) placed_cols, thumb

  let column_joins =
    let join = Bridge.cols ~columns in
    Model.union [ join 0 1; join 1 2; join 2 3; join 3 4 ]

  let support_bridges =
    let bridge c k =
      Bridge.keys
        (Map.find_exn (Map.find_exn columns c).keys k)
        (Map.find_exn (Map.find_exn columns (c + 1)).keys k)
    in
    Model.union [ bridge 0 0; bridge 1 0; bridge 2 2; bridge 3 2 ]

  let bez_wall =
    Wall.column_drop
      ~spacing
      ~columns
      ~z_off:0.
      ~d1:2.
      ~d2:5.
      ~thickness:3.5
      ~n_steps:5
      ~kind:`Poly

  let siding = Wall.poly_siding ~d1:2. ~d2:5. ~thickness:3.5 ~n_steps:5
  let thumb_siding = Wall.poly_siding ~d1:2. ~d2:5. ~thickness:3.5 ~n_steps:5

  let scad =
    Model.union
      [ Model.union
          (Map.fold
             ~f:(fun ~key:_ ~data l -> data.scad :: l)
             ~init:[ thumb.scad ]
             columns )
      ; (bez_wall `North 0).scad
      ; (bez_wall `North 1).scad
      ; (bez_wall `North 2).scad
      ; (bez_wall `South 2).scad
      ; (bez_wall `North 3).scad
      ; (bez_wall `South 3).scad
      ; (bez_wall `North 4).scad
      ; (bez_wall `South 4).scad
      ; (siding `West (Map.find_exn (Map.find_exn columns 0).keys 0)).scad
        (* ; (siding `West (Map.find_exn (Map.find_exn columns 0).keys 1)).scad
         * ; (siding `West (Map.find_exn (Map.find_exn columns 0).keys 2)).scad *)
        (* ; (siding `East (Map.find_exn (Map.find_exn columns 4).keys 2)).scad *)
      ; (thumb_siding `West (Map.find_exn thumb.keys 0)).scad
      ; (thumb_siding `North (Map.find_exn thumb.keys 0)).scad
      ; (thumb_siding `South (Map.find_exn thumb.keys 0)).scad
      ; (thumb_siding `South (Map.find_exn thumb.keys 2)).scad
      ; support_bridges
      ]

  let t = { scad; columns; thumb }
end

let niz_sensor = Sensor.(make Config.a3144)

let niz_platform =
  (* NOTE: BKE ~= 0.95mm; DES ~= 0.73mm. A value of 1.15 seems to fit both without
   * being too tight or loose on either. *)
  Niz.Platform.(
    make
      { w = 20.
      ; dome_w = 19.
      ; dome_waist = 15. (* width at narrow point, ensure enough space at centre *)
      ; dome_thickness = 1.15
      ; base_thickness = 2.25
      ; sensor_depth = 1.5
      ; snap_clearance = 0.3
      ; snap_len = 0.7
      ; lug_height = 1.5
      ; sensor_config = Sensor.Config.a3144_print
      })

let niz_cross_section =
  Model.difference
    (Model.union
       [ Model.translate
           (0., 0., niz_platform.wall_height +. (keyhole.config.thickness /. 2.))
           keyhole.scad
       ; niz_platform.scad
       ] )
    [ Model.cube ~center:true (25., 15., 20.) |> Model.translate (0., -7.5, 0.) ]
