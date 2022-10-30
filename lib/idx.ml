open Syntax

type t =
  | First
  | Last
  | Idx of int

let to_find = function
  | First -> IMap.min_binding_opt >> Option.map snd
  | Last -> IMap.max_binding_opt >> Option.map snd
  | Idx i -> IMap.find_opt i
