open! Base
open! Scad_ml

type 'k t = 'k Column.t Map.M(Int).t

let translate p = Map.map ~f:(Column.translate p)
let mirror ax = Map.map ~f:(Column.mirror ax)
let rotate r = Map.map ~f:(Column.rotate r)
let rotate_about_pt r p = Map.map ~f:(Column.rotate_about_pt r p)
let key t col key = Option.(Map.find t col >>= fun c -> Map.find c.Column.keys key)
let key_exn t col key = Map.find_exn (Map.find_exn t col).Column.keys key
