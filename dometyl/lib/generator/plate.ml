open Base
open Scad_ml
open Infix

module Lookups = struct
  type t =
    { offset : int -> V3.t
    ; curve : int -> Curvature.t
    ; swing : int -> float
    ; splay : int -> float
    ; rows : int -> int
    ; centre : int -> float
    }

  let default_offset = function
    | 2 -> v3 0. 3.5 (-6.) (* middle *)
    | 3 -> v3 1. (-2.5) 0.5 (* ring *)
    | i when i >= 4 -> v3 1. (-22.) 9.5 (* pinky *)
    | 0 -> v3 (-2.) 0. 7.
    | _ -> v3 0. 0. 1.5

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

  let body
      ?(offset = default_offset)
      ?(curve = default_curve)
      ?(swing = default_swing)
      ?(splay = default_splay)
      ?(rows = default_rows)
      ?(centre = default_centre)
      ()
    =
    { offset; curve; swing; splay; rows; centre }

  let thumb
      ?(offset = fun _ -> V3.zero)
      ?(curve =
        fun _ ->
          Curvature.(curve ~fan:{ angle = Float.pi /. 12.5; radius = 85.; tilt = 0. } ()))
      ?(swing = fun _ -> 0.)
      ?(splay = fun _ -> 0.)
      ?(rows = fun _ -> 3)
      ?(centre = fun _ -> 1.)
      ()
    =
    { offset; curve; swing; splay; rows; centre }
end

type config =
  { n_body_rows : int -> int
  ; body_centres : int -> float
  ; n_body_cols : int
  ; centre_col : int
  ; spacing : float
  ; tent : float
  ; n_thumb_rows : int -> int
  ; thumb_centres : int -> float
  ; n_thumb_cols : int
  ; thumb_offset : V3.t
  ; thumb_angle : V3.t
  }

type t =
  { config : config [@scad.ignore]
  ; scad : Scad.d3
  ; body : Columns.t
  ; thumb : Columns.t
  }
[@@deriving scad]

let make
    ?(n_body_cols = 5)
    ?(centre_col = 2)
    ?(spacing = 1.)
    ?(tent = Float.pi /. 12.)
    ?(n_thumb_cols = 1)
    ?(rotate_thumb_clips = false)
    ?(thumb_offset = v3 (-14.) (-42.) 13.5)
    ?(thumb_angle = Float.(v3 (pi /. 20.) (pi /. -9.) (pi /. 12.)))
    ?(body_lookups = Lookups.body ())
    ?(thumb_lookups = Lookups.thumb ())
    ?(caps = Caps.SA.uniform)
    ?thumb_caps
    (keyhole : Key.t)
  =
  let curve_column ~join_ax ~caps (lookups : _ Lookups.t) i k =
    Column.make
      ~join_ax
      ~n_keys:(lookups.rows i)
      ~curve:(Curvature.apply ~centre_idx:(lookups.centre i) (lookups.curve i))
      ~caps
      k
  in
  let body_offsets, thumb_offsets =
    let space = keyhole.config.outer_w +. spacing in
    let f (lookups : _ Lookups.t) m i =
      let data = V3.xtrans (space *. Float.of_int i) (lookups.offset i) in
      Map.add_exn ~key:i ~data m
    and init = Map.empty (module Int) in
    ( List.fold ~f:(f body_lookups) ~init (List.range 0 n_body_cols)
    , List.fold ~f:(f thumb_lookups) ~init (List.range 0 n_thumb_cols) )
  in
  let centre_offset = Map.find_exn body_offsets centre_col in
  let body, thumb =
    let body_cols =
      let f ~key:i ~data:off =
        let tented =
          Column.(
            yrot
              ~about:V3.(centre_offset -@ off)
              tent
              (curve_column ~join_ax:`NS ~caps body_lookups i keyhole))
        in
        Column.rotate
          ~about:(Map.find_exn tented.keys (Int.of_float @@ body_lookups.centre i)).origin
          (v3 0. (body_lookups.swing i) (body_lookups.splay i))
          tented
        |> Column.translate off
      in
      Map.mapi ~f body_offsets
    and thumb =
      let place ~key:i ~data:off =
        let caps, keyhole =
          if rotate_thumb_clips
          then caps >> Scad.zrot (Float.pi /. 2.), Key.zrot (Float.pi /. 2.) keyhole
          else caps, Key.cycle_faces keyhole
        in
        Column.rotate
          (v3 0. (thumb_lookups.swing i) (thumb_lookups.splay i))
          (curve_column
             ~join_ax:`EW
             ~caps:(Option.value ~default:caps thumb_caps)
             thumb_lookups
             i
             keyhole )
        |> Column.translate off
      in
      let placed =
        Map.mapi ~f:place thumb_offsets
        (* orient along x-axis *)
        |> Columns.zrot (Float.pi /. -2.)
        |> Columns.rotate thumb_angle
        |> Columns.translate thumb_offset
      in
      let origin =
        let col = Map.find_exn placed 0 in
        (Map.find_exn col.keys (Int.of_float @@ thumb_lookups.centre 0)).origin
      in
      Columns.(yrot ~about:V3.(centre_offset -@ origin) tent placed)
    in
    let lift =
      let lowest_z =
        let face_low ({ points = ps; _ } : Key.Face.t) =
          Points.fold ~f:(fun m p -> Float.min m (V3.get_z p)) ~init:Float.max_value ps
        in
        let key_low ({ faces = fs; _ } : Key.t) =
          Key.Faces.fold
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
          body_cols
      in
      Map.map ~f:(Column.translate (v3 0. 0. (keyhole.config.clearance -. lowest_z)))
    in
    lift body_cols, lift thumb
  in
  { config =
      { n_body_rows = body_lookups.rows
      ; body_centres = body_lookups.centre
      ; n_body_cols
      ; centre_col
      ; spacing
      ; tent
      ; n_thumb_rows = thumb_lookups.rows
      ; thumb_centres = thumb_lookups.centre
      ; n_thumb_cols
      ; thumb_offset
      ; thumb_angle
      }
  ; scad = Scad.union [ Columns.to_scad body; Columns.to_scad thumb ]
  ; body
  ; thumb
  }

let join_thumb ?in_d ?out_d1 ?out_d2 thumb =
  let join = Bridge.cols ~ax:`NS ?in_d ?out_d1 ?out_d2 ~columns:thumb in
  Scad.union_3d (List.init ~f:(fun i -> join i (i + 1)) (Map.length thumb - 1))

let column_joins ?in_d ?out_d1 ?out_d2 { config = { n_body_cols; _ }; body; thumb; _ } =
  let join = Bridge.cols ?in_d ?out_d1 ?out_d2 ~columns:body in
  Scad.union
    [ Scad.union (List.init ~f:(fun i -> join i (i + 1)) (n_body_cols - 1))
    ; join_thumb ?in_d ?out_d1 ?out_d2 thumb
    ]

let skeleton_bridges
    ?in_d
    ?out_d1
    ?out_d2
    { config = { n_body_rows; n_body_cols; _ }; body; thumb; _ }
  =
  let bridge c first =
    let r = if first then fun _ -> 0 else fun i -> n_body_rows i - 1 in
    match Columns.key body c (r c) with
    | Some ({ origin = o1; _ } as k1) ->
      let%bind.Option next_col = Map.find body (c + 1) in
      let f ~key:_ ~data:(Key.{ origin = o2; _ } as k2) (m, closest) =
        let dist = V3.distance o1 o2 in
        if Float.(dist < m) then dist, Some k2 else m, closest
      in
      let _, k2 = Map.fold ~init:(Float.max_value, None) ~f next_col.keys in
      Option.map ~f:(Bridge.keys ?in_d ?out_d1 ?out_d2 k1) k2
    | None -> None
  in
  ignore thumb;
  Scad.union
    [ List.init ~f:(fun i -> bridge i (i < 2)) (n_body_cols - 1)
      |> List.filter_opt
      |> Scad.union
    ; join_thumb ?in_d ?out_d1 ?out_d2 thumb
    ]

let to_scad t = t.scad

let collect ~f t =
  let col_fold =
    Map.fold ~f:(fun ~key:_ ~data acc -> Map.fold ~init:acc ~f data.Column.keys)
  in
  Scad.union_3d @@ col_fold ~init:(col_fold ~init:[] t.body) t.thumb

let collect_caps t =
  collect
    ~f:(fun ~key:_ ~data acc ->
      Option.value_map ~default:acc ~f:(fun c -> c :: acc) data.Key.cap )
    t

let collect_cutouts t =
  collect
    ~f:(fun ~key:_ ~data acc ->
      Option.value_map ~default:acc ~f:(fun c -> c :: acc) data.Key.cutout )
    t
