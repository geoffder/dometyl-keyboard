open! Base
open! Scad_ml

val prism_exn : Vec3.t list -> Vec3.t list -> Model.t

val bisection_exn
  :  ?max_iter:int
  -> tolerance:float
  -> f:(float -> float)
  -> float
  -> float
  -> float

val prepend_opt : 'a option -> 'a list -> 'a list
val prepend_opt_map : f:('a -> 'b) -> 'a option -> 'b list -> 'b list
val fill_points : ?init:Vec3.t list -> n:int -> Vec3.t -> Vec3.t -> Vec3.t list
