open! Base
open! Scad_ml

module Join : sig
  module Faces : sig
    type t =
      { west : Scad.d3
      ; east : Scad.d3
      }

    val map : f:(Scad.d3 -> Scad.d3) -> t -> t
    val translate : Vec3.t -> t -> t
    val rotate : Vec3.t -> t -> t
    val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t
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

val make
  :  ?join_ax:[< `EW | `NS > `NS ]
  -> n_keys:int
  -> curve:(int -> 'k KeyHole.t -> 'k KeyHole.t)
  -> caps:(int -> Scad.d3)
  -> 'k KeyHole.t
  -> 'k t
