open! Base
open! Scad_ml

type 'k t = 'k Column.t Map.M(Int).t

val translate : Vec3.t -> 'k t -> 'k t
val rotate : Vec3.t -> 'k t -> 'k t
val rotate_about_pt : Vec3.t -> Vec3.t -> 'k t -> 'k t
val key : 'k t -> int -> int -> 'k KeyHole.t option
val key_exn : 'k t -> int -> int -> 'k KeyHole.t
