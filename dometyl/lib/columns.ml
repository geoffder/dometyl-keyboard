open! Base
open! Scad_ml

type 'k t = 'k Column.t Map.M(Int).t

let key_exn t col key = Map.find_exn (Map.find_exn t col).Column.keys key
