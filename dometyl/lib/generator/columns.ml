open! Base
open! Scad_ml

type t = (Column.t Map.M(Int).t[@scad.mapf] [@scad.d3]) [@@deriving scad]

let key t col key = Option.(Map.find t col >>= fun c -> Map.find c.Column.keys key)
let key_exn t col key = Map.find_exn (Map.find_exn t col).Column.keys key

let to_scad t =
  Scad.union3 (Map.fold ~init:[] ~f:(fun ~key:_ ~data l -> Column.to_scad data :: l) t)
