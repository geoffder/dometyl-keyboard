open Base
open Scad_ml
open Infix

module Lookups = struct
  type 'k t =
    { offset : int -> Vec3.t
    ; curve : int -> 'k Curvature.t
    ; swing : int -> float
    ; splay : int -> float
    ; rows : int -> int
    ; centre : int -> float
    }

  let default_offset = function
    | 2 -> 0., 3.5, -6. (* middle *)
    | 3 -> 1., -2.5, 0.5 (* ring *)
    | i when i >= 4 -> 1., -22., 9.5 (* pinky *)
    | 0 -> -2., 0., 7.
    | _ -> 0., 0., 1.5

  let default_curve = function
    | i when i = 3 ->
      Curvature.(curve ~well:(spec ~radius:37. (Float.pi /. 4.5)) ()) (* ring *)
    | i when i > 3 ->
      Curvature.(curve ~well:(spec ~radius:35. (Float.pi /. 4.1)) ()) (* pinky *)
    | i when i = 0 ->
      Curvature.(
        curve ~well:(spec ~tilt:(Float.pi /. 6.75) ~radius:45. (Float.pi /. 6.)) ())
    | _ -> Curvature.(curve ~well:(spec ~radius:46. (Float.pi /. 6.3)) ())

  (* post tenting, this can be used to undo tent angle (y-rotation) *)
  let default_swing _ = 0.

  let default_splay = function
    | i when i = 3 -> Float.pi /. -25. (* ring *)
    | i when i >= 4 -> Float.pi /. -9. (* pinky *)
    | _ -> 0.

  let default_rows _ = 3
  let default_centre _ = 1.

  let make
      ?(offset = default_offset)
      ?(curve = default_curve)
      ?(swing = default_swing)
      ?(splay = default_splay)
      ?(rows = default_rows)
      ?(centre = default_centre)
      ()
    =
    { offset; curve; swing; splay; rows; centre }
end

type 'k config =
  { n_rows : int -> int
  ; row_centres : int -> float
  ; n_cols : int
  ; centre_col : int
  ; spacing : float
  ; tent : float
  ; thumb_offset : Vec3.t
  ; thumb_angle : Vec3.t
  }

type 'k t =
  { config : 'k config [@scad.ignore]
  ; scad : Scad.t
  ; columns : 'k Column.t Map.M(Int).t [@scad.mapf]
  ; thumb : 'k Column.t
  }
[@@deriving scad]

let make_thumb ~n_keys ~centre_idx ~curve ~caps ~rotate_clips keyhole =
  Column.(
    make
      ~join_ax:`EW
      ~n_keys
      ~curve:(Curvature.apply ~centre_idx curve)
      ~caps:(if rotate_clips then caps >> Scad.rotate (0., 0., Float.pi /. 2.) else caps)
      ( if rotate_clips
      then KeyHole.rotate (0., 0., Float.pi /. 2.) keyhole
      else KeyHole.cycle_faces keyhole )
    (* orient along x-axis *)
    |> rotate (0., 0., Float.pi /. -2.))

let make
    ?(n_cols = 5)
    ?(centre_col = 2)
    ?(spacing = 1.)
    ?(tent = Float.pi /. 12.)
    ?(n_thumb_keys = 3)
    ?(thumb_centre = 1.)
    ?(thumb_curve =
      Curvature.(
        curve
          ~fan:{ angle = Float.pi /. 8.8; radius = 70.; tilt = Float.pi /. 48. }
          ~well:{ angle = Float.pi /. 5.5; radius = 47.; tilt = 0. })
        ())
    ?(rotate_thumb_clips = false)
    ?(thumb_offset = -14., -42., 13.5)
    ?(thumb_angle = Float.(pi /. 20., pi /. -9., pi /. 12.))
    ?(lookups = Lookups.make ())
    ?(caps = Caps.SA.uniform)
    ?thumb_caps
    (keyhole : _ KeyHole.t)
  =
  let curve_column i =
    Column.make
      ~n_keys:(lookups.rows i)
      ~curve:(Curvature.apply ~centre_idx:(lookups.centre i) (lookups.curve i))
      ~caps
      keyhole
  in
  let col_offsets =
    let space = keyhole.config.outer_w +. spacing in
    let f m i =
      let data = Vec3.(lookups.offset i <+> (space *. Float.of_int i, 0., 0.)) in
      Map.add_exn ~key:i ~data m
    in
    List.fold ~f ~init:(Map.empty (module Int)) (List.range 0 n_cols)
  in
  let centre_offset = Map.find_exn col_offsets centre_col in
  let apply_tent off col =
    Column.(rotate_about_pt (0., tent, 0.) Vec3.(off <-> centre_offset) col)
  in
  let place_col ~key:i ~data:off =
    let tented = apply_tent off (curve_column i) in
    Column.rotate_about_pt
      (0., lookups.swing i, lookups.splay i)
      (Vec3.negate (Map.find_exn tented.keys (Int.of_float @@ lookups.centre i)).origin)
      tented
    |> Column.translate off
  in
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
      Column.translate (0., 0., keyhole.config.clearance -. lowest_z)
    in
    let thumb =
      let placed =
        Column.(
          make_thumb
            ~n_keys:n_thumb_keys
            ~centre_idx:thumb_centre
            ~curve:thumb_curve
            ~caps:(Option.value ~default:caps thumb_caps)
            ~rotate_clips:rotate_thumb_clips
            keyhole
          |> rotate thumb_angle
          |> translate thumb_offset)
      in
      apply_tent (Map.find_exn placed.keys 1).origin placed |> lift
    in
    Map.map ~f:lift placed_cols, thumb
  in
  { config =
      { n_rows = lookups.rows
      ; row_centres = lookups.centre
      ; n_cols
      ; centre_col
      ; spacing
      ; tent
      ; thumb_offset
      ; thumb_angle
      }
  ; scad =
      Scad.union
        (Map.fold ~f:(fun ~key:_ ~data l -> data.scad :: l) ~init:[ thumb.scad ] columns)
  ; columns
  ; thumb
  }

let column_joins ?in_d ?out_d1 ?out_d2 { config = { n_cols; _ }; columns; _ } =
  let join = Bridge.cols ?in_d ?out_d1 ?out_d2 ~columns in
  Scad.union (List.init ~f:(fun i -> join i (i + 1)) (n_cols - 1))

let skeleton_bridges ?in_d ?out_d1 ?out_d2 { config = { n_rows; n_cols; _ }; columns; _ } =
  let bridge c first =
    let r = if first then fun _ -> 0 else fun i -> n_rows i - 1 in
    Option.map2
      ~f:(Bridge.keys ?in_d ?out_d1 ?out_d2)
      (Columns.key columns c (r c))
      (Columns.key columns (c + 1) (r (c + 1)))
  in
  List.init ~f:(fun i -> bridge i (i < 2)) (n_cols - 1) |> List.filter_opt |> Scad.union

let to_scad t = t.scad

let collect ~f t =
  let body =
    Map.fold
      ~init:[]
      ~f:(fun ~key:_ ~data acc -> Map.fold ~init:acc ~f data.Column.keys)
      t.columns
  in
  Scad.union @@ Map.fold ~init:body ~f t.thumb.keys

let collect_caps t =
  collect
    ~f:(fun ~key:_ ~data acc ->
      Option.value_map ~default:acc ~f:(fun c -> c :: acc) data.KeyHole.cap )
    t

let collect_cutouts t =
  collect
    ~f:(fun ~key:_ ~data acc ->
      Option.value_map ~default:acc ~f:(fun c -> c :: acc) data.KeyHole.cutout )
    t
