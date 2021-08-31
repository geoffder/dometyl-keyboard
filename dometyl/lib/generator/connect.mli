open! Base
open! Scad_ml

type t =
  { scad : Model.t
  ; outline : Vec3.t list
  ; inline : Vec3.t list
  }

val bounding_box : t -> float * float * float * float
val centre : float * float * float * float -> float * float
val translate : Vec3.t -> t -> t
val rotate : Vec3.t -> t -> t
val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t

val prism_connection
  :  (float -> Vec3.t) list
  -> [< `Ragged of int list | `Uniform of int ]
  -> t

val clockwise_union : t list -> t
val outline_2d : t -> (float * float) list
val inline_2d : t -> (float * float) list
val base_endpoints : height:float -> [< `Left | `Right ] -> Wall.t -> Vec3.t list
val base_steps : n_steps:int -> Vec3.t list -> Vec3.t list -> [> `Ragged of int list ]
val bez_base : ?height:float -> ?n_steps:int -> Wall.t -> Wall.t -> t

val cubic_base
  :  ?height:float
  -> ?scale:float
  -> ?d:float
  -> ?n_steps:int
  -> ?bow_out:bool
  -> Wall.t
  -> Wall.t
  -> t

val snake_base
  :  ?height:float
  -> ?scale:float
  -> ?d:float
  -> ?n_steps:int
  -> Wall.t
  -> Wall.t
  -> t

val inward_elbow_base : ?height:float -> ?n_steps:int -> ?d:float -> Wall.t -> Wall.t -> t

val straight_base
  :  ?height:float
  -> ?fudge_factor:float
  -> ?min_width:float
  -> Wall.t
  -> Wall.t
  -> t

val join_walls : ?n_steps:int -> ?fudge_factor:float -> Wall.t -> Wall.t -> t

val joiner
  :  get:('a -> Wall.t option)
  -> join:(Wall.t -> Wall.t -> t)
  -> key:_
  -> data:'a
  -> Wall.t option * t list
  -> Wall.t option * t list

val skeleton
  :  ?index_height:float
  -> ?height:float
  -> ?min_straight_width:float
  -> ?n_steps:int
  -> ?body_join_steps:int
  -> ?thumb_join_steps:int
  -> ?join_index:bool
  -> ?fudge_factor:float
  -> ?join_fudge_factor:float
  -> ?snake_d:float
  -> ?snake_scale:float
  -> ?cubic_d:float
  -> ?cubic_scale:float
  -> ?thumb_cubic_d:float
  -> ?thumb_cubic_scale:float
  -> ?thumb_height:float
  -> ?pinky_idx:int
  -> ?close_thumb:bool
  -> ?close_pinky:bool
  -> Walls.t
  -> t

val closed
  :  ?join_west:bool
  -> ?n_steps:int
  -> ?fudge_factor:float
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
