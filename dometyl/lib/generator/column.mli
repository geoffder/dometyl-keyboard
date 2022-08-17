open! Base
open! Scad_ml

module Join : sig
  (** A hull between {!Key.t}s of a column, and thin scads for each of the outer faces
      (available for hulling between columns in {!module:Bridge}.). *)

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

type config =
  { key : Key.t
  ; n_keys : int
  ; curve : int -> Key.t -> Key.t
  }

type t =
  { config : config
  ; scad : Scad.d3
  ; keys : Key.t Map.M(Int).t
  ; joins : Join.t Map.M(Int).t
  }
[@@deriving scad]

(** [make ?join_ax ~n_keys ~curve ~caps key]

    Create a column by distributing [n_keys] copies of [key] using the distribution
    function [curve], which will position each {!Key.t} according to the index (from 0
    to [n_keys - 1]), before they are joined via {!Scad.hull} between the {!Key.faces}
    along the axis indicated by [join_ax]. The [caps] lookup places a key cap above each
    {!Key.t} corresponding to the index (examples/defaults can be found in
    {!module:Caps}). *)
val make
  :  ?join_ax:[< `EW | `NS > `NS ]
  -> n_keys:int
  -> curve:(int -> Key.t -> Key.t)
  -> caps:(int -> Scad.d3)
  -> Key.t
  -> t

val to_scad : t -> Scad.d3
