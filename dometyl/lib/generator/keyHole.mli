open! Base
open! Scad_ml

module Face : sig
  (** An outer face of a keyhole. *)

  type t =
    { scad : Scad.d3  (** Thin wall for the purposes of hulling to other faces. *)
    ; points : Points.t  (** Corner and centre points of the face. *)
    }
  [@@deriving scad]

  val make : Vec3.t -> t
  val direction : t -> Vec3.t
end

module Faces : sig
  (** The four outer faces of a keyhole. *)
  type t =
    { north : Face.t
    ; south : Face.t
    ; east : Face.t
    ; west : Face.t
    }
  [@@deriving scad]

  val map : f:(Face.t -> Face.t) -> t -> t
  val fold : f:('k -> Face.t -> 'k) -> init:'k -> t -> 'k
  val make : float -> float -> float -> t
  val face : t -> [< `East | `North | `South | `West ] -> Face.t
end

module Kind : sig
  type key = Key
  type 'k t = Key : key t
end

type 'k config =
  { spec : 'k Kind.t
  ; outer_w : float
  ; outer_h : float
  ; inner_w : float
  ; inner_h : float
  ; thickness : float
  ; clip : Scad.d3 -> Scad.d3
  ; cap_height : float
  ; clearance : float
  }

type 'k t =
  { config : 'k config [@scad.ignore]
  ; scad : Scad.d3
  ; origin : Vec3.t
  ; faces : Faces.t
  ; cap : Scad.d3 option
  ; cutout : Scad.d3 option
  }
[@@deriving scad]

val rotate_about_origin : Vec3.t -> 'k t -> 'k t
val quaternion_about_origin : float -> 'k t -> 'k t
val cycle_faces : 'k t -> 'k t
val orthogonal : 'k t -> [< `East | `North | `South | `West ] -> Vec3.t
val normal : 'k t -> Vec3.t
val make : ?render:bool -> ?cap:Scad.d3 -> ?cutout:Scad.d3 -> 'k config -> 'k t
val mirror_internals : 'k t -> 'k t
val cutout_scad : 'k t -> Scad.d3
