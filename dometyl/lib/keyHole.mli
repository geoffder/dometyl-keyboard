open! Base
open! Scad_ml

module Face : sig
  type t =
    { scad : Model.t
    ; points : Points.t
    }

  val make : Vec3.t -> t
  val translate : Vec3.t -> t -> t
  val rotate : Vec3.t -> t -> t
  val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t
  val quaternion : Quaternion.t -> t -> t
  val quaternion_about_pt : Quaternion.t -> Vec3.t -> t -> t
  val direction : t -> Vec3.t
end

module Faces : sig
  type t =
    { north : Face.t
    ; south : Face.t
    ; east : Face.t
    ; west : Face.t
    }

  val map : f:(Face.t -> Face.t) -> t -> t
  val fold : f:('k -> Face.t -> 'k) -> init:'k -> t -> 'k
  val make : float -> float -> float -> t
  val face : t -> [< `East | `North | `South | `West ] -> Face.t
  val translate : Vec3.t -> t -> t
  val rotate : Vec3.t -> t -> t
  val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t
  val quaternion : Quaternion.t -> t -> t
  val quaternion_about_pt : Quaternion.t -> Vec3.t -> t -> t
end

module Kind : sig
  type niz =
    { clip_height : float
    ; snap_slot_h : float
    }

  type mx = unit

  type _ t =
    | Mx : mx -> mx t
    | Niz : niz -> niz t
end

type 'k config =
  { spec : 'k Kind.t
  ; outer_w : float
  ; outer_h : float
  ; inner_w : float
  ; inner_h : float
  ; thickness : float
  ; clip : Model.t -> Model.t
  ; cap_height : float
  ; clearance : float
  }

type 'k t =
  { config : 'k config
  ; scad : Model.t
  ; origin : Vec3.t
  ; faces : Faces.t
  ; cap : Model.t option
  ; cutout : Model.t option
  }

val translate : Vec3.t -> 'k t -> 'k t
val rotate : Vec3.t -> 'k t -> 'k t
val rotate_about_pt : Vec3.t -> Vec3.t -> 'k t -> 'k t
val quaternion : Quaternion.t -> 'k t -> 'k t
val quaternion_about_pt : Quaternion.t -> Vec3.t -> 'k t -> 'k t
val rotate_about_origin : Vec3.t -> 'k t -> 'k t
val quaternion_about_origin : float -> 'k t -> 'k t
val cycle_faces : 'k t -> 'k t
val orthogonal : 'k t -> [< `East | `North | `South | `West ] -> Vec3.t
val normal : 'k t -> Vec3.t
val make : ?cap:Model.t -> ?cutout:Model.t -> 'k config -> 'k t
