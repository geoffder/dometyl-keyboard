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
    -> west:(int -> Wall.config option)
    -> north:(int -> Wall.config option)
    -> east:(int -> Wall.config option)
    -> south:(int -> Wall.config option)
    -> 'k Columns.t
    -> t

  val make :
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

val make_body :
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

val make_thumb :
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

val manual :
     body_west:(int -> Wall.config option)
  -> body_north:(int -> Wall.config option)
  -> body_east:(int -> Wall.config option)
  -> body_south:(int -> Wall.config option)
  -> thumb_south:(int -> Wall.config option)
  -> thumb_north:(int -> Wall.config option)
  -> thumb_east:(int -> Wall.config option)
  -> thumb_west:(int -> Wall.config option)
  -> 'k Plate.t
  -> t

val to_scad : t -> Scad.d3
val collect_screws : t -> Eyelet.t list
