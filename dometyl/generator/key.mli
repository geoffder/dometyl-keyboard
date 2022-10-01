open! Scad_ml

module Face : sig
  (** An outer face of a keyhole. *)

  type t =
    { path : Path3.t (** Path outlining the face. *)
    ; points : Points.t
          (** Corner and centre points of the face. If the face is rounded,
                these are the points before the roundover begins. *)
    ; bounds : Points.t (** Original unrounded corner points. *)
    ; normal : V3.t (** Normal vector *)
    }
  [@@deriving scad]

  val direction : t -> V3.t
end

module Faces : sig
  (** The four outer faces of a keyhole. *)
  type t =
    { north : Face.t [@scad.d3]
    ; south : Face.t
    ; east : Face.t
    ; west : Face.t
    }
  [@@deriving scad]

  val map : (Face.t -> Face.t) -> t -> t
  val fold : ('k -> Face.t -> 'k) -> 'k -> t -> 'k
  val face : t -> [< `East | `North | `South | `West ] -> Face.t
end

type config =
  { outer_w : float
  ; outer_h : float
  ; inner_w : float
  ; inner_h : float
  ; thickness : float
  ; clip : Scad.d3 -> Scad.d3
  ; cap_height : float
  ; clearance : float
  ; corner : Path3.Round.corner option
  ; fn : int option
  }

type t =
  { config : config [@scad.ignore]
  ; scad : Scad.d3
  ; origin : V3.t
  ; faces : Faces.t
  ; cap : Scad.d3 option
  ; cutout : Scad.d3 option
  }
[@@deriving scad]

val rotate_about_origin : V3.t -> t -> t
val quaternion_about_origin : float -> t -> t
val cycle_faces : t -> t
val orthogonal : t -> [< `East | `North | `South | `West ] -> V3.t
val normal : t -> V3.t
val make : ?render:bool -> ?cap:Scad.d3 -> ?cutout:Scad.d3 -> config -> t
val mirror_internals : t -> t
val cutout_scad : t -> Scad.d3
