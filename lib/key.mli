open OCADml
open OSCADml

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
  [@@deriving cad]

  val direction : t -> V3.t
end

module Faces : sig
  (** The four outer faces of a keyhole. *)
  type t =
    { north : Face.t [@cad.d3]
    ; south : Face.t
    ; east : Face.t
    ; west : Face.t
    }
  [@@deriving cad]

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
  { config : config [@cad.ignore]
  ; scad : Scad.d3
  ; origin : V3.t
  ; faces : Faces.t
  ; cap : Scad.d3 option
  ; cutout : Scad.d3 option
  }
[@@deriving cad]

(** [rotate_about_origin r t]

    Rotate the keyhole [t] about its origin by the (euler) angles [r]. *)
val rotate_about_origin : V3.t -> t -> t

(** [quaternion_about_origin a t]

    Rotate the keyhole [t] about its normal by [a] radians. *)
val quaternion_about_origin : float -> t -> t

(** [cycle_faces t]

    Swap the faces of the keyhole [t] in the clockwise direction. *)
val cycle_faces : t -> t

(** [orthogonal t side]

    Compute the orthogonal of the face at the given [side] of keyhole [t]. *)
val orthogonal : t -> [< `East | `North | `South | `West ] -> V3.t

(** [normal t]

    Compute the normal of the plane that the keyhole [t] rests on. *)
val normal : t -> V3.t

val make : ?render:bool -> ?cap:Scad.d3 -> ?cutout:Scad.d3 -> config -> t

(** [mirror_internals t]

    Mirror interal cutouts/shapes of the keyhole [t]. *)
val mirror_internals : t -> t

(** [to_scad t]

    Obtain the final scad from the keyhole [t], subtracting out the [cutout] if
    there is one. *)
val to_scad : t -> Scad.d3
