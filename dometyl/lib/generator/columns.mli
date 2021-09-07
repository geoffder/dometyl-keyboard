open! Base
open! Scad_ml

type 'k t = 'k Column.t Map.M(Int).t

include Sigs.Transformable' with type 'k t := 'k t

val key : 'k t -> int -> int -> 'k KeyHole.t option
val key_exn : 'k t -> int -> int -> 'k KeyHole.t
