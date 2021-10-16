open! Base
open! Scad_ml

module Join : sig
  (** A hull between {!KeyHole.t}s of a column, and thin scads for each of the
      outer faces (available for hulling between columns in
      {!module:Bridge}.). *)

  module Faces : sig
    type t =
      { west : Scad.d3
      ; east : Scad.d3
      }
    [@@deriving scad]

    val map : f:(Scad.d3 -> Scad.d3) -> t -> t
    val face : t -> [< `East | `West ] -> Scad.d3
  end

  type t =
    { scad : Scad.d3
    ; faces : Faces.t
    }
  [@@deriving scad]
end

type 'k config =
  { key : 'k KeyHole.t
  ; n_keys : int
  ; curve : int -> 'k KeyHole.t -> 'k KeyHole.t
  }

type 'k t =
  { config : 'k config
  ; scad : Scad.d3
  ; keys : 'k KeyHole.t Map.M(Int).t
  ; joins : Join.t Map.M(Int).t
  }
[@@deriving scad]

(** [make ?join_ax ~n_keys ~curve ~caps key]

    Create a column by distributing [n_keys] copies of [key] using the
    distribution function [curve], which will position each {!KeyHole.t}
    according to the index (from 0 to [n_keys - 1]), before they are joined via
    {!Scad.hull} between the {!KeyHole.faces} along the axis indicated by
    [join_ax]. The [caps] lookup places a key cap above each {!KeyHole.t}
    corresponding to the index (examples/defaults can be found in
    {!module:Caps}). *)
val make
  :  ?join_ax:[< `EW | `NS > `NS ]
  -> n_keys:int
  -> curve:(int -> 'k KeyHole.t -> 'k KeyHole.t)
  -> caps:(int -> Scad.d3)
  -> 'k KeyHole.t
  -> 'k t
