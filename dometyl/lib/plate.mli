open! Base
open! Scad_ml

module Lookups : sig
  type 'k t =
    { offset : int -> Vec3.t
    ; curve : int -> 'k Curvature.t
    ; splay : int -> float
    }

  val default_offset : int -> Vec3.t
  val default_curve : int -> 'k Curvature.t
  val default_splay : int -> float

  val make
    :  ?offset:(int -> Vec3.t)
    -> ?curve:(int -> 'k Curvature.t)
    -> ?splay:(int -> float)
    -> unit
    -> 'k t
end

type 'k config =
  { n_rows : int
  ; centre_row : int
  ; n_cols : int
  ; centre_col : int
  ; spacing : float
  ; tent : float
  ; thumb_offset : Vec3.t
  ; thumb_angle : Vec3.t
  }

type 'k t =
  { config : 'k config
  ; scad : Model.t
  ; columns : 'k Column.t Map.M(Int).t
  ; thumb : 'k Column.t
  }

val translate : Vec3.t -> 'k t -> 'k t
val rotate : Vec3.t -> 'k t -> 'k t
val rotate_about_pt : Vec3.t -> Vec3.t -> 'k t -> 'k t

val make_thumb
  :  curve:(int -> 'k KeyHole.t -> 'k KeyHole.t)
  -> rotate_clips:bool
  -> 'k KeyHole.t
  -> 'k Column.t

val make
  :  ?n_rows:int
  -> ?centre_row:int
  -> ?n_cols:int
  -> ?centre_col:int
  -> ?spacing:float
  -> ?tent:float
  -> ?thumb_offset:Vec3.t
  -> ?thumb_angle:Vec3.t
  -> ?thumb_curve:(int -> 'k KeyHole.t -> 'k KeyHole.t)
  -> ?rotate_thumb_clips:bool
  -> ?lookups:'k Lookups.t
  -> 'k KeyHole.t
  -> 'k t

val column_joins : 'k t -> Model.t
val skeleton_bridges : 'k t -> Model.t
val to_scad : 'k t -> Model.t

val collect
  :  f:(key:int -> data:'k KeyHole.t -> Model.t list -> Model.t list)
  -> 'k t
  -> Model.t

val collect_caps : 'k t -> Model.t
val collect_cutouts : 'k t -> Model.t
