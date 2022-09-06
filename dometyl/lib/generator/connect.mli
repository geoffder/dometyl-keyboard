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

val full_join : ?fudge_factor:float -> ?overlap_factor:float -> unit -> config

val spline
  :  ?height:float
  -> ?fn:int
  -> ?corner_fn:int
  -> ?corner:Path3.Round.corner
  -> unit
  -> config

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
  :  ?index_height:float
  -> ?height:float
  -> ?fn:int
  -> ?corner_fn:int
  -> ?corner:Path3.Round.corner
  -> ?fudge_factor:float
  -> ?overlap_factor:float
  -> ?thumb_height:float
  -> ?east_link:config
  -> ?west_link:config
  -> ?north_joins:(int -> bool)
  -> ?south_joins:(int -> bool)
  -> ?close_thumb:bool
  -> Walls.t
  -> t

val closed
  :  ?fudge_factor:float
  -> ?overlap_factor:float
  -> ?height:float
  -> ?fn:int
  -> ?corner_fn:int
  -> ?corner:Path3.Round.corner
  -> ?west_link:config
  -> ?east_link:config
  -> Walls.t
  -> t

val to_scad : t -> Scad.d3
