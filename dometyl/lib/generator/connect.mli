open! Base
open! Scad_ml

(** Shape(s) connecting walls together, and the points along the outside and inside of the
    foot. *)
type t =
  { scad : Scad.d3
  ; outline : Path3.t
  ; inline : Path3.t
  }
[@@deriving scad]

type config

(** [clockwise_union ts] Create a union of [ts]. They should be provided in clockwise
    order, such that the outline and inline of the resulting {!t} are continous and in the
    clockwise direction. *)
val clockwise_union : t list -> t

(** [outline_2d t]

    Retrieve the outline points of [t] and project them to 2d vectors. *)
val outline_2d : t -> Path2.t

(** [inline_2d t]

    Retrieve the inline points of [t] and project them to 2d vectors. *)
val inline_2d : t -> Path2.t

val straight
  :  ?n_facets:int
  -> ?height:float
  -> ?fudge_factor:float
  -> ?overlap_factor:float
  -> ?min_width:float
  -> unit
  -> config

val bez : ?n_facets:int -> ?height:float -> ?n_steps:int -> unit -> config

val cubic
  :  ?n_facets:int
  -> ?height:float
  -> ?scale:float
  -> ?d:float
  -> ?n_steps:int
  -> ?bow_out:bool
  -> unit
  -> config

val snake
  :  ?n_facets:int
  -> ?height:float
  -> ?scale:float
  -> ?d:float
  -> ?n_steps:int
  -> unit
  -> config

val full_join
  :  ?n_steps:Wall.Steps.t
  -> ?fudge_factor:float
  -> ?overlap_factor:float
  -> unit
  -> config

val elbow : ?n_facets:int -> ?height:float -> ?n_steps:int -> ?d:float -> unit -> config
val spline : ?height:float -> ?n_steps:int -> unit -> config

val manual
  :  ?west:(int -> config)
  -> ?north:(int -> config)
  -> ?south:(int -> config)
  -> ?east:(int -> config)
  -> ?east_link:config
  -> ?thumb_east:(int -> config)
  -> ?thumb_south:(int -> config)
  -> ?thumb_west:(int -> config)
  -> ?thumb_north:(int -> config)
  -> ?west_link:config
  -> Walls.t
  -> t

val skeleton
  :  ?n_facets:int
  -> ?index_height:float
  -> ?height:float
  -> ?min_straight_width:float
  -> ?n_steps:int
  -> ?body_join_steps:Wall.Steps.t
  -> ?thumb_join_steps:Wall.Steps.t
  -> ?fudge_factor:float
  -> ?join_fudge_factor:float
  -> ?overlap_factor:float
  -> ?cubic_d:float
  -> ?cubic_scale:float
  -> ?thumb_height:float
  -> ?east_link:config
  -> ?west_link:config
  -> ?north_joins:(int -> bool)
  -> ?south_joins:(int -> bool)
  -> ?pinky_idx:int
  -> ?pinky_elbow:bool
  -> ?close_thumb:bool
  -> Walls.t
  -> t

val closed
  :  ?body_steps:Wall.Steps.t
  -> ?thumb_steps:Wall.Steps.t
  -> ?fudge_factor:float
  -> ?overlap_factor:float
  -> ?west_link:config
  -> ?east_link:config
  -> Walls.t
  -> t

val to_scad : t -> Scad.d3
