open! Base
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

(** Bezier curve wall edge type and helpers. *)
module Edge : sig
  (** Bezier function, returns position along curve, from 0. to 1. *)
  type t = float -> V3.t [@@deriving scad]

  (** [point_at_z ?max_iter ?tolerance t z]

      Use {!Util.bisection_exn} to search for the {!V3.t} in [t] closest to [z].
      [max_iter] and [tolerance] provide maximum iterations and tolerance (accuracy)
      bounds to the search function. *)
  val point_at_z : ?max_iter:int -> ?tolerance:float -> t -> float -> V3.t
end

(** Functions that find the point along the top and bottom edges of the start position of
    the wall closest to the given xy position, and provides a bezier edge to the ground
    starting from there. *)
module EdgeDrawer : sig
  type drawer = V3.t -> Edge.t

  type t =
    { top : drawer [@scad.d3]
    ; bot : drawer
    }
  [@@deriving scad]

  val make
    :  ?max_iter:int
    -> ?tolerance:float
    -> get_bez:(bool -> V3.t -> Edge.t)
    -> Points.t
    -> t

  val map : f:(drawer -> drawer) -> t -> t
end

(** Bezier curves representing the four edges running from the start to the foot of each
    {!Wall.t} *)
module Edges : sig
  (** Record containting an {!Edge.t} running from the corners of {!Key.Face.points}
      down to the floor, where the terminal points are used for {!Wall.foot}. *)
  type t =
    { top_left : Edge.t [@scad.d3]
    ; top_right : Edge.t
    ; bot_left : Edge.t
    ; bot_right : Edge.t
    }
  [@@deriving scad]

  val map : f:(Edge.t -> Edge.t) -> t -> t

  (** [of_cw_path_exn l] Convert a four element list into a [t]. The ordering
      actually shouldn't just be clockwise, but is assumed to literally be: TL, TR, BR,
      BL. *)
  val of_cw_path_exn : Edge.t list -> t

  val of_cw_path : Edge.t list -> (t, string) Result.t

  (** [get t corner]

      Access fields from record [t] according to provided [corner] tag. *)
  val get : t -> [< `BL | `BR | `TL | `TR ] -> Edge.t
end

type config =
  { d1 : float
  ; d2 : float
  ; thickness : float
  ; clearance : float
  ; n_steps : Steps.t
  ; scale : V2.t option
  ; scale_ez : (V2.t * V2.t) option
  ; n_facets : int
  ; eyelet_config : Eyelet.config option
  }

val default : config

(** Record representing a wall extending from a {!Key.Face.t} to the ground. *)
type t =
  { scad : Scad.d3 (** Aggregate scad, including screw outshoot if included *)
  ; start : Points.t (** Corner points of the {!Key.Face.t} this wall emerged from *)
  ; foot : Points.t (** Terminal points where the wall meets the XY plane. *)
  ; edge_drawer : EdgeDrawer.t
        (** Generate {!Edge.t}'s emerging from point along top and bottom starting edges
            of the wall closest to the provided {!V3.t} on the XY plane. *)
  ; edges : Edges.t (** Bezier curves that specify the edge vertices. *)
  ; screw : Eyelet.t option
        (** Scad, coordinates, and config of screw offshoot if included. *)
  }
[@@deriving scad]

(** [swing_face key_orgin face]

    Find a rotation around [face]s bottom or top axis, depending on which way
    it is pointing in z (determined with [key_origin]), that brings [face] to a more
    vertical orientation, returning a pivoted {!Key.Face.t} and it's new orthogonal
    {!V3.t}. *)
val swing_face : V3.t -> Key.Face.t -> Key.Face.t * V3.t

(** [poly_siding ?x_off ?y_off ?clearance ?n_steps ?n_facets ?d1 ?d2 ?thickness
      ?eyelet_config side keyhole]

    Generate a {!type:t} using an OpenScad polyhedron, drawn from a set of bezier curves
    from the [side] facing edges of [keyhole]. Optional parameters influence the shape of
    the generated wall:

    - [x_off] and [y_off] shift the target endpoints (on the ground) of the wall
    - [clearance] moves the start of the wall out from the face of the keyhole
    - [n_steps] controls the number of points used to draw the wall (see:
      {!module:Steps}). This impact the aeshetics of the wall, but it also determines how
      well the wall follows the bezier curve, which can have implications for positioning
      the cutouts for ports near the bottom of the case. A number of steps that is too
      high can sometimes cause the generated polyhedrons to fail, as points can bunch up,
      leading to a mesh that is difficult for the OpenScad engine (CGAL) to close. When
      this happens, either decreasing number of steps (can be done preferentially for
      short walls with `PerZ), or increasing [n_facets] to increase the number of (and
      decrease the size of) triangles that CGAL can use to close the wall shape.
    - [n_facets] sets the number of polyhedron faces assigned to the outside and inside of
      the wall. Values above one will introduce additional beziers of vertices spaced
      between the ends of the wall, leading to a finer triangular mesh.
    - [d1] and [d2] set the distance projected outward along the orthogonal of the [side]
      of the [keyhole] on the xy-plane used to for the second and third quadratic bezier
      control points respectively.
    - [thickness] influences the thickness of the wall (from inside to outside face)
    - If provided, [eyelet_config] describes the screw/bumpon eyelet that should be added
      to the bottom of the generated wall. *)
val poly_siding
  :  ?x_off:float
  -> ?y_off:float
  -> ?clearance:float
  -> ?n_steps:[< `Flat of int | `PerZ of float > `Flat ]
  -> ?n_facets:int
  -> ?d1:float
  -> ?d2:float
  -> ?thickness:float
  -> ?scale:V2.t
  -> ?scale_ez:V2.t * V2.t
  -> ?eyelet_config:Eyelet.config
  -> [< `East | `North | `South | `West ]
  -> Key.t
  -> t

val poly_of_config
  :  ?x_off:float
  -> ?y_off:float
  -> config
  -> [< `East | `North | `South | `West ]
  -> Key.t
  -> t

(** [column_drop ?clearance ?n_steps ?n_facets ?d1 ?d2 ?thickness ?eyelet_config
      ~spacing ~columns idx]

    Wrapper function for {!val:poly_siding} specifically for (north and south) column end
    walls. Unlike {!val:poly_siding}, which takes a {!Key.t}, this takes the map
    [columns], and an [idx] specifying the column to generate the wall for. Overhang over
    the next column (to the right) that may have been introduced by tenting is checked
    for, and an x offset that will reclaim the desired column [spacing] is calculated and
    passed along to {!val:poly_siding}. *)
val column_drop
  :  ?clearance:float
  -> ?n_steps:[< `Flat of int | `PerZ of float > `Flat ]
  -> ?n_facets:int
  -> ?d1:float
  -> ?d2:float
  -> ?thickness:float
  -> ?scale:V2.t
  -> ?scale_ez:V2.t * V2.t
  -> ?eyelet_config:Eyelet.config
  -> spacing:float
  -> columns:Columns.t
  -> [< `North | `South ]
  -> int
  -> t

val drop_of_config
  :  spacing:float
  -> config
  -> columns:Columns.t
  -> [< `North | `South ]
  -> int
  -> t

(** [start_direction t]

    Direction vector from right to left of the wall start points. *)
val start_direction : t -> V3.t

(** [foot_direction t]

    Direction vector from right to left of the wall foot points. *)
val foot_direction : t -> V3.t

val to_scad : t -> Scad.d3
