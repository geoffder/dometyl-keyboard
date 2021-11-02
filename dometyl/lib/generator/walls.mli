open! Base
open! Scad_ml

(** Used for lookups, indicating whether to place a wall at a particular position on the
    plate, and if so, whether there should be an eyelet attached. *)
type presence =
  | No
  | Yes
  | Eye

module Side : sig
  type t = Wall.t Map.M(Int).t [@@deriving scad]
  type config = int -> Wall.config option
end

module Sides : sig
  type t =
    { west : Side.t
    ; north : Side.t
    ; east : Side.t
    ; south : Side.t
    }
  [@@deriving scad]

  val manual_body :
       ?spacing:float
    -> west:Side.config
    -> north:Side.config
    -> east:Side.config
    -> south:Side.config
    -> 'k Columns.t
    -> t

  val manual_thumb :
       west:Side.config
    -> north:Side.config
    -> east:Side.config
    -> south:Side.config
    -> 'k Columns.t
    -> t

  val auto :
       ?d1:float
    -> ?d2:float
    -> ?z_off:float
    -> ?thickness:float
    -> ?index_thickness:float
    -> ?north_clearance:float
    -> ?south_clearance:float
    -> ?side_clearance:float
    -> ?n_steps:Wall.Steps.t
    -> ?n_facets:int
    -> ?north_lookup:(int -> presence)
    -> ?south_lookup:(int -> presence)
    -> ?west_lookup:(int -> presence)
    -> ?east_lookup:(int -> presence)
    -> ?eyelet_config:Eyelet.config
    -> ?spacing:float
    -> ?thumb:bool
    -> 'k Columns.t
    -> t

  val get : t -> [ `N | `E | `S | `W ] -> Side.t
  val to_scad : t -> Scad.d3
  val collect_screws : ?init:Eyelet.t list -> t -> Eyelet.t list
end

val auto_body :
     ?d1:float
  -> ?d2:float
  -> ?z_off:float
  -> ?thickness:float
  -> ?index_thickness:float
  -> ?north_clearance:float
  -> ?south_clearance:float
  -> ?side_clearance:float
  -> ?n_steps:Wall.Steps.t
  -> ?n_facets:int
  -> ?north_lookup:(int -> presence)
  -> ?south_lookup:(int -> presence)
  -> ?west_lookup:(int -> presence)
  -> ?east_lookup:(int -> presence)
  -> ?eyelet_config:Eyelet.config
  -> 'k Plate.t
  -> Sides.t

val auto_thumb :
     ?d1:float
  -> ?d2:float
  -> ?z_off:float
  -> ?thickness:float
  -> ?index_thickness:float
  -> ?north_clearance:float
  -> ?south_clearance:float
  -> ?side_clearance:float
  -> ?n_steps:Wall.Steps.t
  -> ?n_facets:int
  -> ?north_lookup:(int -> presence)
  -> ?south_lookup:(int -> presence)
  -> ?west_lookup:(int -> presence)
  -> ?east_lookup:(int -> presence)
  -> ?eyelet_config:Eyelet.config
  -> 'k Plate.t
  -> Sides.t

type t =
  { body : Sides.t
  ; thumb : Sides.t
  }
[@@deriving scad]

val make : body:Sides.t -> thumb:Sides.t -> t

val manual :
     body_west:Side.config
  -> body_north:Side.config
  -> body_east:Side.config
  -> body_south:Side.config
  -> thumb_south:Side.config
  -> thumb_north:Side.config
  -> thumb_east:Side.config
  -> thumb_west:Side.config
  -> 'k Plate.t
  -> t

val to_scad : t -> Scad.d3
val collect_screws : t -> Eyelet.t list
