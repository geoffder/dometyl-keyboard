open! Base
open! Scad_ml

(** Configuration type for number of steps used to draw the bezier curves that
    form the walls. *)
module Steps : sig
  (** [`Flat n] simply indicates [n] steps, [`PerZ mm] specifies that there should
      be a step for every [mm] off the ground the start of the wall is. *)
  type t =
    [ `Flat of int
    | `PerZ of float
    ]

  (** [to_int t z]

      Converts to a discrete number of steps. In the case of `Flat, it is simply the
      contained integer and the z value will be ignored. For `PerZ, the resulting
      number of steps will be how many times the wrapped float fits into the provided
      z value. *)
  val to_int : [< `Flat of int | `PerZ of float ] -> float -> int
end

(** Bezier curve wall edge type and helpers. *)
module Edge : sig
  (** Bezier function, returns position along curve, from 0. to 1. *)
  type t = float -> Vec3.t

  (** Basic transformation functions *)
  include Sigs.Transformable with type t := t

  (** [point_at_z ?max_iter ?tolerance t z]

      Use {!Util.bisection_exn} to search for the {!Vec3.t} in [t] closest to
      [z]. [max_iter] and [tolerance] provide maximum iterations and tolerance
      (accuracy) bounds to the search function. *)
  val point_at_z : ?max_iter:int -> ?tolerance:float -> t -> float -> Vec3.t
end

(** Bezier curves representing the four edges running from the start to the foot
    of each {!Wall.t} *)
module Edges : sig
  (** Record containting an {!Edge.t} running from the corners of
      {!KeyHole.Face.points} down to the floor, where the terminal points are
      used for {!Wall.foot}. *)
  type t =
    { top_left : Edge.t
    ; top_right : Edge.t
    ; bot_left : Edge.t
    ; bot_right : Edge.t
    }

  val map : f:(Edge.t -> Edge.t) -> t -> t

  (** Basic transformation functions *)
  include Sigs.Transformable with type t := t

  (** [of_clockwise_list_exn l]
      Convert a four element list into a [t]. The ordering actually shouldn't just be
      clockwise, but is assumed to literally be: TL, TR, BR, BL. *)
  val of_clockwise_list_exn : Edge.t list -> t

  val of_clockwise_list : Edge.t list -> (t, string) Result.t

  (** [get t corner]

      Access fields from record [t] according to provided [corner] tag. *)
  val get : t -> [< `BL | `BR | `TL | `TR ] -> Edge.t
end

(** Record representing a wall extending from a {!KeyHole.Face.t} to the ground. *)
type t =
  { scad : Model.t (** Aggregate scad, including screw outshoot if included *)
  ; start : Points.t (** Corner points of the {!KeyHole.Face.t} this wall emerged from *)
  ; foot : Points.t (** Terminal points where the wall meets the XY plane. *)
  ; edges : Edges.t (** Bezier curves that specify the edge vertices. *)
  ; screw : Screw.t option
        (** Model, coordinates, and config of screw offshoot if included. *)
  }

(** Basic transformation functions *)
include Sigs.Transformable with type t := t

(** [swing_face ?step key_orgin face]

    Iteratively find a rotation around [face]s bottom or top axis, depending on
    which way it is pointing in z (determined with [key_origin]), that brings
    [face] to a more vertical orientation, returning a pivoted {!KeyHole.Face.t}
    and it's new orthogonal {!Vec3.t}. *)
val swing_face : ?step:float -> Vec3.t -> KeyHole.Face.t -> KeyHole.Face.t * Vec3.t

(** [] *)
val poly_siding
  :  ?x_off:float
  -> ?y_off:float
  -> ?z_off:float
  -> ?clearance:float
  -> ?n_steps:[< `Flat of int | `PerZ of float > `Flat ]
  -> ?n_facets:int
  -> ?d1:float
  -> ?d2:float
  -> ?thickness:float
  -> ?screw_config:Screw.config
  -> [< `East | `North | `South | `West ]
  -> 'a KeyHole.t
  -> t

(** [] *)
val column_drop
  :  ?z_off:float
  -> ?clearance:float
  -> ?d1:float
  -> ?d2:float
  -> ?thickness:float
  -> ?n_steps:[< `Flat of int | `PerZ of float > `Flat ]
  -> ?n_facets:int
  -> ?screw_config:Screw.config
  -> spacing:float
  -> columns:'k Columns.t
  -> [< `North | `South ]
  -> int
  -> t

(** [start_direction t]

    Direction vector from right to left of the wall start points. *)
val start_direction : t -> Vec3.t

(** [foot_direction t]

    Direction vector from right to left of the wall foot points. *)
val foot_direction : t -> Vec3.t

val to_scad : t -> Model.t
