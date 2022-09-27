open Scad_ml

type loc =
  | Thumb of Util.idx * Util.idx
  | Body of Util.idx * Util.idx
  | Point of v2
  | U of float

type t = (Eyelet.t list[@scad.d3]) [@@deriving scad]

(* let of_locs ?walls ?connect locs = *)
(*   let f acc = function *)
(*     | Point p -> Eyelet.Point p *)
(*     | *)
(*     in *)
