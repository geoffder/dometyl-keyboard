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
  let tented_z theta (x, _, z) = (z *. Float.cos theta) +. (-.x *. Float.sin theta)

  (* TODO: this seems to work, but need to do the tougher task of getting the
   * lowest thumb cluster position. Need to have a more complete rotation function
   * than the simple tented_z which only handles rotation around the y axis *)
  let lowest_z =
    let f ~key:_ ~data:(x, y, z) low =
      tented_z
        tent
        ( x +. (Col.Key.outer_w /. 2.) -. Util.get_x centre_offset
        , y
        , z -. (Col.Key.thickness /. 2.) )
      |> Float.min low
    in
    Map.fold ~f ~init:Float.max_value col_offsets

  (* TODO: The x offset subtraction fixed an issue the keyhole crossing the xaxis
   * to be gimped, but I should probably shift the whole model over to avoid
   * any rotation problems all together. (Though this fix is fine to stay in place
   * I think. ) *)
  let place_col off =
    let f =
      Model.translate off
      >> Util.(rotate_about_pt (0., tent, 0.) (get_x off -. get_x centre_offset, 0., 0.))
    in
    Col.map ~f Col.t

  let columns = Map.map ~f:place_col col_offsets

  let thumb =
    Thumb.map
      ~f:(fun t -> Util.(t |@> thumb_angle |>> (thumb_offset <+> (0., 0., rise))))
      Thumb.t

  let scad =
    Model.union
      (Map.fold
         ~f:(fun ~key:_ ~data l -> data.scad :: l)
         ~init:
           [ thumb.scad; Model.cube (1., 1., 1.) |> Model.translate (0., 0., lowest_z) ]
         columns )

  let t = { scad; columns; thumb }
end
