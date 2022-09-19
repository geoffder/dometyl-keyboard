open Scad_ml
open Syntax

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
      Curvature.(curve ~well:(well ~radius:37. (Float.pi /. 4.5)) ()) (* ring *)
    | i when i > 3 ->
      Curvature.(curve ~well:(well ~radius:35. (Float.pi /. 4.1)) ()) (* pinky *)
    | i when i = 0 ->
      Curvature.(
        curve ~well:(well ~tilt:(Float.pi /. 6.75) ~radius:45. (Float.pi /. 6.)) ())
    | _ -> Curvature.(curve ~well:(well ~radius:46. (Float.pi /. 6.3)) ())

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
      ?(curve = fun _ -> Curvature.(curve ~fan:(fan ~radius:85. (Float.pi /. 12.5)) ()))
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
  let curve_column ~join_ax ~caps (lookups : Lookups.t) i k =
    Column.make
      ~join_ax
      ~n_keys:(lookups.rows i)
      ~curve:(Curvature.apply ~centre_idx:(lookups.centre i) (lookups.curve i))
      ~caps
      k
  in
  let body_offsets, thumb_offsets =
    let space = keyhole.config.outer_w +. spacing in
    let f (lookups : Lookups.t) m i =
      let data = V3.xtrans (space *. Float.of_int i) (lookups.offset i) in
      IMap.add i data m
    and init = IMap.empty in
    ( Seq.fold_left (f body_lookups) init (Seq.init n_body_cols Fun.id)
    , Seq.fold_left (f thumb_lookups) init (Seq.init n_thumb_cols Fun.id) )
  in
  let centre_offset = IMap.find centre_col body_offsets in
  let body, thumb =
    let body_cols =
      let f i off =
        let tented =
          Column.(
            yrot
              ~about:V3.(centre_offset -@ off)
              tent
              (curve_column ~join_ax:`NS ~caps body_lookups i keyhole))
        in
        Column.rotate
          ~about:(IMap.find (Int.of_float @@ body_lookups.centre i) tented.keys).origin
          (v3 0. (body_lookups.swing i) (body_lookups.splay i))
          tented
        |> Column.translate off
      in
      IMap.mapi f body_offsets
    and thumb =
      let place i off =
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
        IMap.mapi place thumb_offsets
        (* orient along x-axis *)
        |> Columns.zrot (Float.pi /. -2.)
        |> Columns.rotate thumb_angle
        |> Columns.translate thumb_offset
      in
      let origin =
        let col = IMap.find 0 placed in
        (IMap.find (Int.of_float @@ thumb_lookups.centre 0) col.keys).origin
      in
      Columns.(yrot ~about:V3.(centre_offset -@ origin) tent placed)
    in
    let lift =
      let lowest_z =
        let face_low ({ points = ps; _ } : Key.Face.t) =
          Points.fold (fun m p -> Float.min m (V3.get_z p)) Float.max_float ps
        in
        let key_low ({ faces = fs; _ } : Key.t) =
          Key.Faces.fold (fun m face -> Float.min m (face_low face)) Float.max_float fs
        in
        let col_low ({ keys = ks; _ } : Column.t) =
          IMap.fold (fun _key data m -> Float.min m (key_low data)) ks Float.max_float
        in
        IMap.fold (fun _ data m -> Float.min m (col_low data)) body_cols Float.max_float
      in
      IMap.map (Column.translate (v3 0. 0. (keyhole.config.clearance -. lowest_z)))
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

let col_skip skip i = List.filter_map (fun (c, r) -> if c = i then Some r else None) skip

let join_thumb ?in_d ?out_d1 ?out_d2 ?(skip = []) ?(join_skip = []) thumb =
  let join = Bridge.cols ~ax:`NS ?in_d ?out_d1 ?out_d2 ~columns:thumb in
  let f i = join ~skip:(col_skip skip i) ~join_skip:(col_skip join_skip i) i (i + 1) in
  Scad.union3 (List.init (IMap.cardinal thumb - 1) f)

let column_joins
    ?in_d
    ?out_d1
    ?out_d2
    ?(body_skip = [])
    ?(body_join_skip = [])
    ?thumb_skip
    ?thumb_join_skip
    { config = { n_body_cols; _ }; body; thumb; _ }
  =
  let join = Bridge.cols ?in_d ?out_d1 ?out_d2 ~columns:body in
  let f i =
    join ~skip:(col_skip body_skip i) ~join_skip:(col_skip body_join_skip i) i (i + 1)
  in
  Scad.union
    [ Scad.union (List.init (n_body_cols - 1) f)
    ; join_thumb ?in_d ?out_d1 ?out_d2 ?skip:thumb_skip ?join_skip:thumb_join_skip thumb
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
      let* next_col = IMap.find_opt (c + 1) body in
      let f _ (Key.{ origin = o2; _ } as k2) (m, closest) =
        let dist = V3.distance o1 o2 in
        if dist < m then dist, Some k2 else m, closest
      in
      let _, k2 = IMap.fold f next_col.keys (Float.max_float, None) in
      Option.map (Bridge.keys ?in_d ?out_d1 ?out_d2 k1) k2
    | None -> None
  in
  let f i acc = Util.prepend_opt (bridge i (i < 2)) acc in
  Scad.union
    [ Scad.union (Util.fold_init (n_body_cols - 1) f [])
    ; join_thumb ?in_d ?out_d1 ?out_d2 thumb
    ]

let to_scad t = t.scad

let collect f t =
  let col_fold = IMap.fold (fun _ data acc -> IMap.fold f data.Column.keys acc) in
  Scad.union3 @@ col_fold t.thumb (col_fold t.body [])

let collect_caps t =
  collect
    (fun _ data acc -> Util.value_map_opt ~default:acc (fun c -> c :: acc) data.Key.cap)
    t

let collect_cutouts t =
  collect
    (fun _ data acc -> Util.value_map_opt ~default:acc (fun c -> c :: acc) data.Key.cutout)
    t
