open! Scad_ml

(** Configuration type for number of steps used to draw the bezier curves that form the
    walls. *)
module Steps : sig
  (** [`Flat n] simply indicates [n] steps, [`PerZ mm] specifies that there should be a
      step for every [mm] off the ground the start of the wall is. *)
  type t =
    [ `Flat of int
    | `PerZ of float
    ]

  (** [to_int t z]

      Converts to a discrete number of steps. In the case of `Flat, it is simply the
      contained integer and the z value will be ignored. For `PerZ, the resulting number
      of steps will be how many times the wrapped float fits into the provided z value. *)
  val to_int : [< `Flat of int | `PerZ of float ] -> float -> int
end

(** Type providing a means to obtain the path from a particular point on the
    originating face of a wall and the ground. *)
module Drawer : sig
  type loc =
    [ `BL
    | `BR
    | `TL
    | `TR
    | `CN
    | `B of float
    | `T of float
    | `L of float
    | `R of float
    | `XY of float * float
    ]

  type t = loc -> Path3.t [@@deriving scad]

  val map : (Path3.t -> Path3.t) -> t -> t
end

type config =
  { d1 : [ `Abs of float | `Rel of float ] option
  ; d2 : float option
  ; clearance : float option
  ; n_steps : Steps.t option
  ; min_step_dist : float option
  ; scale : V2.t option
  ; scale_ez : (V2.t * V2.t) option
  ; end_z : float option
  }

val default : config

(** Record representing a wall extending from a {!Key.Face.t} to the ground. *)
type t =
  { scad : Scad.d3 (** Aggregate scad, including screw outshoot if included *)
  ; key : Key.t
  ; side : [ `North | `East | `South | `West ] [@scad.ignore]
  ; start : Points.t (** Corner points of the {!Key.Face.t} this wall emerged from *)
  ; cleared : Points.t
        (** Corner points of the swung (vertical) and cleared [start] face *)
  ; foot : Points.t (** Terminal points where the wall meets the XY plane. *)
  ; drawer : Drawer.t
        (** Generate {!Path3.t}s emerging from a point on the [start] face that
             follow along the same sweeping transforms as the wall. *)
  ; bounds_drawer : Drawer.t
        (** Same as drawer, but operating within the unrounded bounding corners
            of the face instead. *)
  }
[@@deriving scad]

(** [swing_face face]

    Find a rotation around [face]s bottom or top axis, depending on which way
    it is pointing in z (determined by [face.normal]), that brings [face] to a more
    vertical orientation, returning a pivoted {!Key.Face.t}. *)
val swing_face : Key.Face.t -> Key.Face.t

(** [make side keyhole]

    Generate a {!type:t} using an OpenScad polyhedron, drawn from a set of bezier curves
    from the [side] facing edges of [keyhole]. Optional parameters influence the shape of
    the generated wall:

    - [clearance] moves the start of the wall out from the face of the keyhole
    - [n_steps] controls the number of points used to draw the wall (see:
      {!module:Steps}). This impact the aeshetics of the wall, but it also determines how
      well the wall follows the bezier curve, which can have implications for positioning
      the cutouts for ports near the bottom of the case. A number of steps that is too
      high can sometimes cause the generated polyhedrons to fail, as points can bunch up,
      leading to a mesh with intersecting faces that will cause the OpenScad
      engine (CGAL) to fail. When this happens, either decreasing number of steps
      (can be done preferentially for short walls with `PerZ), or increasing [d1]
      which will spread out the points at the beginning of the sweep (which is
      typically the most problematic) may help.
    - [min_step_dist] sets the minimum distance that each profile (step) of the
      walls sweep must be away from the previous. To avoid self intersecting
      meshes, any steps that are closer than this minimum will be skipped.
    - [d1] and [d2] set the distance projected outward along the orthogonal of the [side]
      of the [keyhole] on the xy-plane used to for the second and third quadratic bezier
      control points respectively.
    - [scale] specifies width ([x]), and thickness ([y]) scaling to apply along
      the sweep of the projected wall (linearly, or easing according to [scale_ez])
    - [end_z] specifies the height above the xy plane the bezier sweep portion
      of the wall should end (from there dropping straight down to lie flat on
      the plane) *)
val make
  :  ?clearance:float
  -> ?n_steps:[< `Flat of int | `PerZ of float > `Flat ]
  -> ?min_step_dist:float
  -> ?d1:[ `Abs of float | `Rel of float ]
  -> ?d2:float
  -> ?scale:V2.t
  -> ?scale_ez:V2.t * V2.t
  -> ?end_z:float
  -> [ `East | `North | `South | `West ]
  -> Key.t
  -> t

val of_config : config -> [ `East | `North | `South | `West ] -> Key.t -> t

(** [start_direction t]

    Direction vector from right to left of the wall start points. *)
val start_direction : t -> V3.t

(** [foot_direction t]

    Direction vector from right to left of the wall foot points. *)
val foot_direction : t -> V3.t

val to_scad : t -> Scad.d3
