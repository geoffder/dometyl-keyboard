open Base
open Scad_ml

module type Config = sig
  val n_keys : int

  module Key : KeyHole.S
  module Curve : Curvature.S
end

module type S = sig
  include Config

  type t =
    { scad : Model.t
    ; keys : Key.t Map.M(Int).t
    ; joins : Model.t Map.M(Int).t
    }

  val map : f:(Model.t -> Model.t) -> t -> t
  val t : t
end

module Make (C : Config) = struct
  include C

  type t =
    { scad : Model.t
    ; keys : Key.t Map.M(Int).t
    ; joins : Model.t Map.M(Int).t
    }

  let map ~f t =
    { scad = f t.scad; keys = Map.map ~f:(Key.map ~f) t.keys; joins = Map.map ~f t.joins }

  let place_keys keys i = Map.add_exn ~key:i ~data:(Key.map ~f:(Curve.place i) Key.t) keys
  let join_keys (a : Key.t) (b : Key.t) = Model.hull [ a.north_face; b.south_face ]
  let keys = List.fold (List.range 0 n_keys) ~init:(Map.empty (module Int)) ~f:place_keys

  let joins =
    Map.fold
      ~f:(fun ~key ~data:k1 m ->
        match Map.find keys (key + 1) with
        | None    -> m
        | Some k2 -> Map.add_exn m ~key ~data:(join_keys k1 k2) )
      ~init:(Map.empty (module Int))
      keys

  let scad =
    Model.union
      (Map.fold ~f:(fun ~key:_ ~data l -> data.scad :: l) ~init:(Map.data joins) keys)

  let t = { scad; keys; joins }
end
