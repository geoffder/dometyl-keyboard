(* open Scad_ml *)

(* type loc = *)
(*   | Thumb of [ `N | `E | `S | `W ] * Util.idx *)
(*   | Body of [ `N | `E | `S | `W ] * Util.idx *)
(*   | Point of v3 *)
(*   | U of float *)

type wall_loc =
  | Thumb of [ `N | `E | `S | `W ] * Util.idx
  | Body of [ `N | `E | `S | `W ] * Util.idx

type t = (Eyelet.t list[@scad.d3]) [@@deriving scad]

(* let of_locs ?walls ?connect locs = *)
(*   let f acc = function *)
(*     | Point p -> Eyelet.Point p *)
(*     | *)
(*     in *)
