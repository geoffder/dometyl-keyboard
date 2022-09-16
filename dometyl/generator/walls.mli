open! Scad_ml

(** Used for lookups, indicating whether to place a wall at a particular position on the
    plate, and if so, whether there should be an eyelet attached. *)
type presence =
  | No
  | Yes
  | Eye

module Side : sig
  (** Maps from index/position to {!Wall.t}, representing a side of the body or thumb of a
      plate. *)
  type t = (Wall.t IMap.t[@scad.d3]) [@@deriving scad]

  (** A function taking a index/position along a side of the body or thumb of a plate, and
      returning some {!Wall.config} if there should be a {!Wall.t} made at that location,
      or none if not. *)
  type config = int -> Wall.config option
end

module Sides : sig
  (** Four cardinal {!Side.t}s for a body/thumb. *)
  type t =
    { west : Side.t [@scad.d3]
    ; north : Side.t
    ; east : Side.t
    ; south : Side.t
    }
  [@@deriving scad]

  (** [manual_body ~west ~north ~east ~south columns]

      Construct a {!t} with the given {!Side.config}s for each of the edges of the
      provided {!Plate.t} body [columns] (columns oriented along the Y-axis (N-S)). *)
  val manual_body
    :  west:Side.config
    -> north:Side.config
    -> east:Side.config
    -> south:Side.config
    -> Columns.t
    -> t

  (** [manual_thumb ~west ~north ~east ~south columns]

      Construct a {!t} with the given {!Side.config}s for each of the edges of the
      provided {!Plate.t} thumb [columns] (columns oriented along the X-axis (W-E)). Note
      that the relevant indices are increment in directions reflecting the 90 degree
      rotation of the thumb columns relative to the body. *)
  val manual_thumb
    :  west:Side.config
    -> north:Side.config
    -> east:Side.config
    -> south:Side.config
    -> Columns.t
    -> t

  (** [auto columns]

      Construct a {!t} from a collection of parameters to be applied consistently across
      the generated {!Wall.t}s.

      - [d1], [d2], [n_steps], [scale], [scale_ez], and [eyelet_config] are as described in
        the documentation of {!Wall.poly_siding}
      - [index_scale] can be used to override [scale] for the first two columns of
        the body.
      - [{north,south,side}_clearance] provide clearance to {!Wall.poly_siding} for the
        corresponding sections.
      - [thumb] flags whether [columns] represents a thumb plate (default is false)
      - [{north,south,west,east}_lookup] parameters are functions from index to
        {!presence}, indicating whether there is a wall present at a position or not (and
        if there should be an eyelet attached to its base) *)
  val auto
    :  ?d1:[ `Abs of float | `Rel of float ]
    -> ?d2:float
    -> ?north_clearance:float
    -> ?south_clearance:float
    -> ?side_clearance:float
    -> ?n_steps:Wall.Steps.t
    -> ?min_step_dist:float
    -> ?scale:V2.t
    -> ?scale_ez:V2.t * V2.t
    -> ?end_z:float
    -> ?index_scale:V2.t
    -> ?north_lookup:(int -> presence)
    -> ?south_lookup:(int -> presence)
    -> ?west_lookup:(int -> presence)
    -> ?east_lookup:(int -> presence)
    -> ?eyelet_config:Eyelet.config
    -> ?thumb:bool
    -> Columns.t
    -> t

  val get : t -> [ `N | `E | `S | `W ] -> Side.t
  val to_scad : t -> Scad.d3
  val collect_screws : ?init:Eyelet.t list -> t -> Eyelet.t list
end

(** Use {!Sides.auto} to generate a {!Sides.t} from the body of the provided {!Plate.t}. *)
val auto_body
  :  ?d1:[ `Abs of float | `Rel of float ]
  -> ?d2:float
  -> ?north_clearance:float
  -> ?south_clearance:float
  -> ?side_clearance:float
  -> ?n_steps:Wall.Steps.t
  -> ?min_step_dist:float
  -> ?scale:V2.t
  -> ?scale_ez:V2.t * V2.t
  -> ?end_z:float
  -> ?index_scale:V2.t
  -> ?north_lookup:(int -> presence)
  -> ?south_lookup:(int -> presence)
  -> ?west_lookup:(int -> presence)
  -> ?east_lookup:(int -> presence)
  -> ?eyelet_config:Eyelet.config
  -> Plate.t
  -> Sides.t

(** Use {!Sides.auto} to generate a {!Sides.t} from the thumb of the provided {!Plate.t}. *)
val auto_thumb
  :  ?d1:[ `Abs of float | `Rel of float ]
  -> ?d2:float
  -> ?north_clearance:float
  -> ?south_clearance:float
  -> ?side_clearance:float
  -> ?n_steps:Wall.Steps.t
  -> ?min_step_dist:float
  -> ?scale:V2.t
  -> ?scale_ez:V2.t * V2.t
  -> ?end_z:float
  -> ?north_lookup:(int -> presence)
  -> ?south_lookup:(int -> presence)
  -> ?west_lookup:(int -> presence)
  -> ?east_lookup:(int -> presence)
  -> ?eyelet_config:Eyelet.config
  -> Plate.t
  -> Sides.t

type t =
  { body : Sides.t [@scad.d3]
  ; thumb : Sides.t
  }
[@@deriving scad]

(** [make ~body ~thumb]

    Construct a {!t} from [body] and [thumb]. *)
val make : body:Sides.t -> thumb:Sides.t -> t

(** [manual ~body_west ~body_north ~body_east ~body_south ~thumb_south
    ~thumb_south ~thumb_north ~thumb_east ~thumb_west plate]

    A helper that passes along the given {!Side.config}s to {!Side.manual_body} and
    {!Side.manual_thumb} to construct a {!t}. *)
val manual
  :  body_west:Side.config
  -> body_north:Side.config
  -> body_east:Side.config
  -> body_south:Side.config
  -> thumb_south:Side.config
  -> thumb_north:Side.config
  -> thumb_east:Side.config
  -> thumb_west:Side.config
  -> Plate.t
  -> t

val to_scad : t -> Scad.d3
val collect_screws : t -> Eyelet.t list
