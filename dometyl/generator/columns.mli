open! OCADml
open! OSCADml

type t = (Column.t IMap.t[@cad.d3]) [@@deriving cad]

val key : t -> int -> int -> Key.t option
val key_exn : t -> int -> int -> Key.t
val to_scad : t -> Scad.d3
