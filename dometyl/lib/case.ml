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
    }

  let n_cols = 5
  let spacing = 1.
  let centre_col = 2
  let tent = Math.pi /. 12.

  (* TODO: tune, these are placeholders *)
  let offsets =
    let space = Col.Key.outer_w +. spacing in
    let lookup = function
      | 2 -> 0., 2.82, -4.5
      | 3 -> 0., 1.5, -2.
      | i when i >= 4 -> 0., -12., 5.64
      | _ -> 0., 0., 0.
    in
    let f m i =
      let data = Util.(lookup i <+> (space *. Float.of_int i, 0., 0.)) in
      Map.add_exn ~key:i ~data m
    in
    List.fold ~f ~init:(Map.empty (module Int)) (List.range 0 n_cols)

  let centre_offset = Map.find_exn offsets centre_col

  (* TODO: The x offset subtraction fixed an issue the keyhole crossing the xaxis
   * to be gimped, but I should probably shift the whole model over to avoid
   * any rotation problems all together. (Though this fix is fine to stay in place
   * I think. ) *)
  let place_col off =
    Col.map
      ~f:
        ( Model.translate off
        >> Util.(
             rotate_about_pt (0., tent, 0.) (get_x off -. get_x centre_offset, 0., 0.)) )
      Col.t

  let columns = Map.map ~f:place_col offsets

  let scad =
    Model.union (Map.fold ~f:(fun ~key:_ ~data l -> data.scad :: l) ~init:[] columns)

  let t = { scad; columns }
end
