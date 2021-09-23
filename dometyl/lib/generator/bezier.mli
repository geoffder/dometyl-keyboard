open! Base
open! Scad_ml

module Rotator : sig
  type t = float -> Scad.t -> Scad.t

  val make : ?pivot:Vec3.t -> Quaternion.t -> Quaternion.t -> t
end

val quad_weights : float -> Vec3.t
val cubic_weights : float -> float * float * float * float
val quad : p1:float -> p2:float -> p3:float -> float -> float
val cubic : p1:float -> p2:float -> p3:float -> p4:float -> float -> float

val quad_vec2
  :  p1:float * float
  -> p2:float * float
  -> p3:float * float
  -> float
  -> float * float

val cubic_vec2
  :  p1:float * float
  -> p2:float * float
  -> p3:float * float
  -> p4:float * float
  -> float
  -> float * float

val quad_vec3 : p1:Vec3.t -> p2:Vec3.t -> p3:Vec3.t -> float -> Vec3.t
val cubic_vec3 : p1:Vec3.t -> p2:Vec3.t -> p3:Vec3.t -> p4:Vec3.t -> float -> Vec3.t
val curve_rev : ?init:'a list -> ?n_steps:int -> (float -> 'a) -> 'a list
val curve : n_steps:int -> (float -> 'a) -> 'a list
val quats : Quaternion.t -> Quaternion.t -> float -> Quaternion.t list

val hull
  :  ?rotator:(float -> Scad.t -> Scad.t)
  -> ?translator:(float -> Scad.t -> Scad.t)
  -> int
  -> Scad.t
  -> Scad.t

val quad_hull
  :  ?rotator:(float -> Scad.t -> Scad.t)
  -> t1:Vec3.t
  -> t2:Vec3.t
  -> t3:Vec3.t
  -> n_steps:int
  -> Scad.t
  -> Scad.t

val cubic_hull
  :  ?rotator:(float -> Scad.t -> Scad.t)
  -> t1:Vec3.t
  -> t2:Vec3.t
  -> t3:Vec3.t
  -> t4:Vec3.t
  -> n_steps:int
  -> Scad.t
  -> Scad.t

val prism_exn
  :  (float -> Vec3.t) list
  -> [< `Ragged of Int.t list | `Uniform of Int.t ]
  -> Scad.t

val prism
  :  (float -> Vec3.t) list
  -> [< `Ragged of Int.t list | `Uniform of Int.t ]
  -> (Scad.t, string) Result.t
