open! Base
open! Scad_ml

type t =
  { scad : Model.t
  ; outline : Vec3.t list
  ; inline : Vec3.t list
  }

val centre : float * float * float * float -> float * float

include Sigs.Transformable with type t := t

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
  -> ?snake_d:float
  -> ?snake_scale:float
  -> ?cubic_d:float
  -> ?cubic_scale:float
  -> ?west_link_cubic:bool
  -> ?thumb_cubic_d:float
  -> ?thumb_cubic_scale:float
  -> ?thumb_height:float
  -> ?north_joins:(int -> bool)
  -> ?south_joins:(int -> bool)
  -> ?pinky_idx:int
  -> ?pinky_elbow:bool
  -> ?close_thumb:bool
  -> Walls.t
  -> t

val closed
  :  ?join_west:bool
  -> ?n_facets:int
  -> ?n_steps:int
  -> ?fudge_factor:float
  -> ?overlap_factor:float
  -> ?snake_d:float
  -> ?snake_scale:float
  -> ?snake_height:float
  -> ?snake_steps:int
  -> ?cubic_height:float
  -> ?cubic_scale:float
  -> ?cubic_d:float
  -> Walls.t
  -> t

val to_scad : t -> Model.t
