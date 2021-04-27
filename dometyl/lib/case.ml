open! Base
open! Scad_ml
open! Infix

module Key = KeyHole.Make (struct
  let outer_w = 19.
  let inner_w = 14.
  let thickness = 4.
end)

module Col = Column.Make (struct
  let n_keys = 3

  module Key = Key

  module Curve = Curvature.Make (struct
    let style = Curvature.Well
    let centre_idx = 1
    let angle = Math.pi /. 12.
    let radius = 85.
  end)
end)

module Thumb = Column.Make (struct
  let n_keys = 3

  module Key = Key

  module Curve = Curvature.Make (struct
    let style = Curvature.Fan
    let centre_idx = 1
    let angle = Math.pi /. 12.
    let radius = 85.
  end)
end)

module Plate = struct
  type t =
    { scad : Model.t
    ; columns : Col.t Map.M(Int).t
    ; thumb : Thumb.t
    }

  let n_cols = 5
  let spacing = 1.
  let centre_col = 2
  let tent = Math.pi /. 12.
  let thumb_offset = -10., -45., 5.
  let thumb_angle = Math.(pi /. 6., pi /. 5., pi /. -3.)

  (* TODO: would like to actually calculate this. Will need to take offsets,
   *  total plate depth, tenting, and required guts clearance into account. *)
  let rise = 18.

  (* TODO: tune, these are placeholders *)
  let col_offsets =
    let space = Col.Key.outer_w +. spacing in
    let lookup = function
      | 2 -> 0., 6., -6. (* middle *)
      | 3 -> 0., 3., -2. (* ring *)
      | i when i >= 4 -> 0., -12., 6. (* pinky *)
      | _ -> 0., 0., 0.
    in
    let f m i =
      let data = Util.(lookup i <+> (space *. Float.of_int i, 0., rise)) in
      Map.add_exn ~key:i ~data m
    in
    List.fold ~f ~init:(Map.empty (module Int)) (List.range 0 n_cols)

  let centre_offset = Map.find_exn col_offsets centre_col

  (* TODO: Consider best way to use this info. Use to set a clearance? Or simply
   * add a warning to the user if this value is too low? *)
  let lowest_z =
    let f ~key:_ ~data:(x, y, z) low =
      Math.rotate_y
        tent
        Util.(
          (x +. (Col.Key.outer_w /. 2.), y, z -. (Col.Key.thickness /. 2.))
          <-> centre_offset)
      |> Util.( <+> ) centre_offset
      |> Util.get_z
      |> Float.min low
    in
    Map.fold ~f ~init:Float.max_value col_offsets

  let bottom_marker = Model.cube (100., 1., 1.) |> Model.translate (0., 0., lowest_z)

  let place_col off =
    Col.(rotate_about_pt (0., tent, 0.) Util.(off <-> centre_offset) t |> translate off)

  let columns = Map.map ~f:place_col col_offsets

  let thumb =
    Thumb.(rotate thumb_angle t |> translate Util.(thumb_offset <+> (0., 0., rise)))

  let scad =
    Model.union
      (Map.fold
         ~f:(fun ~key:_ ~data l -> data.scad :: l )
           (* ~init:[ thumb.scad; bottom_marker ] *)
         ~init:[ thumb.scad ]
         columns )

  let corners =
    let mark p = Model.cube ~center:true (1., 1., 1.) |> Model.translate p in
    Model.union
      (Map.fold
         ~f:(fun ~key:_ ~data l ->
           mark data.origin
           ::
           mark data.faces.south.points.bot_left
           ::
           mark data.faces.south.points.bot_right
           ::
           mark data.faces.south.points.top_left
           ::
           mark data.faces.south.points.centre
           :: mark data.faces.south.points.top_right :: l )
         ~init:[]
         thumb.keys )

  let scad = Model.union [ scad; corners ]

  (* let base =
   *   let full = Model.minkowski [ Model.projection scad; Model.circle 7. ] in
   *   Model.difference full [ Model.offset (`Delta (-10.)) full ] *)
  (* let scad = Model.union [ scad; base ] *)

  let t = { scad; columns; thumb }
end
