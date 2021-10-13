open! Base
open! Scad_ml

type t =
  { scad : Scad.t
  ; outline : Vec3.t list
  ; inline : Vec3.t list
  }
[@@deriving scad]

val centre : float * float * float * float -> float * float

val prism_connection
  :  (float -> Vec3.t) list
  -> [< `Ragged of int list | `Uniform of int ]
  -> t

val clockwise_union : t list -> t
val outline_2d : t -> (float * float) list
val inline_2d : t -> (float * float) list

val facet_points
  :  ?rev:bool
  -> ?n_facets:int
  -> ?init:Vec3.t list
  -> height:float
  -> Wall.Edge.t
  -> Vec3.t list

val base_endpoints
  :  ?n_facets:int
  -> height:float
  -> [< `Left | `Right ]
  -> Wall.t
  -> Vec3.t list

val base_steps : n_steps:int -> Vec3.t list -> Vec3.t list -> [> `Ragged of int list ]
val bez_base : ?n_facets:int -> ?height:float -> ?n_steps:int -> Wall.t -> Wall.t -> t

val cubic_base
  :  ?n_facets:int
  -> ?height:float
  -> ?scale:float
  -> ?d:float
  -> ?n_steps:int
  -> ?bow_out:bool
  -> Wall.t
  -> Wall.t
  -> t

val snake_base
  :  ?n_facets:int
  -> ?height:float
  -> ?scale:float
  -> ?d:float
  -> ?n_steps:int
  -> Wall.t
  -> Wall.t
  -> t

val inward_elbow_base
  :  ?n_facets:int
  -> ?height:float
  -> ?n_steps:int
  -> ?d:float
  -> Wall.t
  -> Wall.t
  -> t

val straight_base
  :  ?n_facets:int
  -> ?height:float
  -> ?fudge_factor:float
  -> ?overlap_factor:float
  -> ?min_width:float
  -> Wall.t
  -> Wall.t
  -> t

val join_walls
  :  ?n_steps:int
  -> ?fudge_factor:float
  -> ?overlap_factor:float
  -> Wall.t
  -> Wall.t
  -> t

val joiner
  :  get:('a -> Wall.t option)
  -> join:(Wall.t -> Wall.t -> t)
  -> key:_
  -> data:'a
  -> Wall.t option * t list
  -> Wall.t option * t list

type config =
  | Straight of
      { n_facets : int option
      ; height : float option
      ; fudge_factor : float option
      ; overlap_factor : float option
      ; min_width : float option
      }
  | Bez of
      { n_facets : int option
      ; height : float option
      ; n_steps : int option
      }
  | Cubic of
      { n_facets : int option
      ; height : float option
      ; scale : float option
      ; d : float option
      ; n_steps : int option
      ; bow_out : bool option
      }
  | Snake of
      { n_facets : int option
      ; height : float option
      ; scale : float option
      ; d : float option
      ; n_steps : int option
      }
  | FullJoin of
      { n_steps : int option
      ; fudge_factor : float option
      ; overlap_factor : float option
      }
  | InwardElbow of
      { n_facets : int option
      ; height : float option
      ; n_steps : int option
      ; d : float option
      }

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
  :  ?n_steps:int
  -> ?fudge_factor:float
  -> ?overlap_factor:float
  -> unit
  -> config

val elbow : ?n_facets:int -> ?height:float -> ?n_steps:int -> ?d:float -> unit -> config
val connect : config -> Wall.t -> Wall.t -> t

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
  -> ?body_join_steps:int
  -> ?thumb_join_steps:int
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
  :  ?n_steps:int
  -> ?fudge_factor:float
  -> ?overlap_factor:float
  -> ?west_link:config
  -> ?east_link:config
  -> Walls.t
  -> t

val to_scad : t -> Scad.t
