open Base
open Scad_ml
open Sigs

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

  include Transformable with type t := t

  val t : t
end

module Make (C : Config) = struct
  include C

  type t =
    { scad : Model.t
    ; keys : Key.t Map.M(Int).t
    ; joins : Model.t Map.M(Int).t
    }

  let translate p t =
    { scad = Model.translate p t.scad
    ; keys = Map.map ~f:(Key.translate p) t.keys
    ; joins = Map.map ~f:(Model.translate p) t.joins
    }

  let rotate r t =
    { scad = Model.rotate r t.scad
    ; keys = Map.map ~f:(Key.rotate r) t.keys
    ; joins = Map.map ~f:(Model.rotate r) t.joins
    }

  let rotate_about_pt r p t =
    { scad = Model.rotate_about_pt r p t.scad
    ; keys = Map.map ~f:(Key.rotate_about_pt r p) t.keys
    ; joins = Map.map ~f:(Model.rotate_about_pt r p) t.joins
    }

  let place_key keys i = Map.add_exn ~key:i ~data:(Curve.place (module Key) i Key.t) keys

  let join_keys (a : Key.t) (b : Key.t) =
    Model.hull [ a.faces.north.scad; b.faces.south.scad ]

  let keys = List.fold (List.range 0 n_keys) ~init:(Map.empty (module Int)) ~f:place_key

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
