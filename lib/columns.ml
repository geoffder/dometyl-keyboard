open! OCADml
open! OSCADml

type t = (Column.t IMap.t[@cad.d3]) [@@deriving cad]

let key t col key =
  Option.bind (IMap.find_opt col t) (fun c -> IMap.find_opt key c.Column.keys)

let key_exn t col key = IMap.find key (IMap.find col t).Column.keys
let to_scad t = Scad.union3 (IMap.fold (fun _key data l -> Column.to_scad data :: l) t [])
