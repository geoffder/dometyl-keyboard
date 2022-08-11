open! Base
open! Scad_ml

type 'k t = ('k Column.t Map.M(Int).t[@scad.d3]) [@@deriving scad]

val key : 'k t -> int -> int -> 'k KeyHole.t option
val key_exn : 'k t -> int -> int -> 'k KeyHole.t
val to_scad : 'k t -> Scad.d3
