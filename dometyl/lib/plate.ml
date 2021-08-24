open Base
open Scad_ml

module Lookups = struct
  type 'k t =
    { offset : int -> Vec3.t
    ; curve : int -> 'k Curvature.t
    ; splay : int -> float
    }

  let default_offset = function
    | 2 -> 0., 4., -6. (* middle *)
    | 3 -> 2.5, -1., 0. (* ring *)
    | i when i >= 4 -> 0.5, -22., 9.5 (* pinky *)
    | 0 -> -2., 0., 7.
    | _ -> 0., 0., 1.5

  let default_curve = function
    | i when i >= 3 ->
      Curvature.(curve ~well:(spec ~radius:50. (Float.pi /. 7.)) ()) (* pinky and ring *)
    | i when i = 0 ->
      Curvature.(
        curve ~well:(spec ~tilt:(Float.pi /. 6.75) ~radius:57.5 (Float.pi /. 8.)) ())
    | _ -> Curvature.(curve ~well:(spec ~radius:60. (Float.pi /. 8.)) ())

  let default_splay = function
    | i when i = 3 -> Float.pi /. -25. (* ring *)
    | i when i >= 4 -> Float.pi /. -15. (* pinky *)
    | _ -> 0.

  let make ?(offset = default_offset) ?(curve = default_curve) ?(splay = default_splay) ()
    =
    { offset; curve; splay }
end

type 'k config =
  { n_rows : int
  ; centre_row : int
  ; n_cols : int
  ; centre_col : int
  ; spacing : float
  ; tent : float
  ; thumb_offset : Vec3.t
  ; thumb_angle : Vec3.t
  }

type 'k t =
  { config : 'k config
  ; scad : Model.t
  ; columns : 'k Column.t Map.M(Int).t
  ; thumb : 'k Column.t
  }

let translate p t =
  { t with
    scad = Model.translate p t.scad
  ; columns = Columns.translate p t.columns
  ; thumb = Column.translate p t.thumb
  }

let rotate r t =
  { t with
    scad = Model.rotate r t.scad
  ; columns = Columns.rotate r t.columns
  ; thumb = Column.rotate r t.thumb
  }

let rotate_about_pt r p t =
  { t with
    scad = Model.rotate_about_pt r p t.scad
  ; columns = Columns.rotate_about_pt r p t.columns
  ; thumb = Column.rotate_about_pt r p t.thumb
  }

let make_thumb ~curve ~rotate_clips keyhole =
  Column.(
    make
      ~join_ax:`EW
      ~n_keys:3
      ~curve
      ( if rotate_clips
      then KeyHole.rotate (0., 0., Float.pi /. 2.) keyhole
      else KeyHole.cycle_faces keyhole )
    (* orient along x-axis *)
    |> rotate (0., 0., Float.pi /. -2.))

let make
    ?(n_rows = 3)
    ?(centre_row = 1)
    ?(n_cols = 5)
    ?(centre_col = 2)
    ?(spacing = 1.)
    ?(tent = Float.pi /. 12.)
    ?(thumb_offset = -16., -44.5, 13.5)
    ?(thumb_angle = Float.(pi /. 12., pi /. -4.75, pi /. 5.5))
    ?(thumb_curve =
      Curvature.(
        place
          ~fan:{ angle = Float.pi /. 9.; radius = 70.; tilt = Float.pi /. 24. }
          ~well:{ angle = Float.pi /. 8.; radius = 50.; tilt = 0. }
          ~centre_idx:1))
    ?(rotate_thumb_clips = false)
    ?(lookups = Lookups.make ())
    (keyhole : _ KeyHole.t)
  =
  let curve_column curvature =
    let curve =
      Curvature.(
        match curvature with
        | Curve { well; fan } -> Curvature.(place ?well ?fan ~centre_idx:centre_row)
        | Custom curve        -> curve)
    in
    Column.make ~n_keys:n_rows ~curve keyhole
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
    let tented = apply_tent off (curve_column @@ lookups.curve i) in
    Column.rotate_about_pt
      (0., 0., lookups.splay i)
      (Vec3.negate (Map.find_exn tented.keys centre_row).origin)
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
          make_thumb ~curve:thumb_curve ~rotate_clips:rotate_thumb_clips keyhole
          |> rotate thumb_angle
          |> translate thumb_offset)
      in
      apply_tent (Map.find_exn placed.keys 1).origin placed |> lift
    in
    Map.map ~f:lift placed_cols, thumb
  in
  { config =
      { n_rows; centre_row; n_cols; centre_col; spacing; tent; thumb_offset; thumb_angle }
  ; scad =
      Model.union
        (Map.fold ~f:(fun ~key:_ ~data l -> data.scad :: l) ~init:[ thumb.scad ] columns)
  ; columns
  ; thumb
  }

let column_joins { config = { n_cols; _ }; columns; _ } =
  let join = Bridge.cols ~columns in
  Model.union (List.init ~f:(fun i -> join i (i + 1)) (n_cols - 1))

let skeleton_bridges { config = { n_rows; n_cols; _ }; columns; _ } =
  let bridge c k =
    Bridge.keys (Columns.key_exn columns c k) (Columns.key_exn columns (c + 1) k)
  in
  Model.union
  @@ List.init
       ~f:(fun i -> if i < 2 then bridge i 0 else bridge i (n_rows - 1))
       (n_cols - 1)

let to_scad t = t.scad

let collect ~f t =
  let body =
    Map.fold
      ~init:[]
      ~f:(fun ~key:_ ~data acc -> Map.fold ~init:acc ~f data.Column.keys)
      t.columns
  in
  Model.union @@ Map.fold ~init:body ~f t.thumb.keys

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
