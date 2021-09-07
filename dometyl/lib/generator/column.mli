open! Base
open! Scad_ml

module Join : sig
  module Faces : sig
    type t =
      { west : Model.t
      ; east : Model.t
      }

    val map : f:(Model.t -> Model.t) -> t -> t
    val translate : Vec3.t -> t -> t
    val rotate : Vec3.t -> t -> t
    val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t
    val face : t -> [< `East | `West ] -> Model.t
  end

  type t =
    { scad : Model.t
    ; faces : Faces.t
    }

  include Sigs.Transformable with type t := t
end

type 'k config =
  { key : 'k KeyHole.t
  ; n_keys : int
  ; curve : int -> 'k KeyHole.t -> 'k KeyHole.t
  }

type 'k t =
  { config : 'k config
  ; scad : Model.t
  ; keys : 'k KeyHole.t Map.M(Int).t
  ; joins : Join.t Map.M(Int).t
  }

include Sigs.Transformable' with type 'k t := 'k t

val make
  :  ?join_ax:[< `EW | `NS > `NS ]
  -> n_keys:int
  -> curve:(int -> 'k KeyHole.t -> 'k KeyHole.t)
  -> 'k KeyHole.t
  -> 'k t
