open Scad_ml

type t = (Column.t IMap.t[@scad.d3]) [@@deriving scad]

val key : t -> int -> int -> Key.t option
val key_exn : t -> int -> int -> Key.t
val to_scad : t -> Scad.d3
