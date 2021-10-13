open! Base
open! Scad_ml

type 'k t = ('k Column.t Map.M(Int).t[@scad.mapf]) [@@deriving scad]

let key t col key = Option.(Map.find t col >>= fun c -> Map.find c.Column.keys key)
let key_exn t col key = Map.find_exn (Map.find_exn t col).Column.keys key
