open! Base
open! Scad_ml

type idx =
  | First
  | Last
  | Idx of int

val rad_to_deg : float -> float
val deg_to_rad : float -> float
val prism_exn : Path3.t -> Path3.t -> Scad.d3

val bisection_exn
  :  ?max_iter:int
  -> tolerance:float
  -> f:(float -> float)
  -> float
  -> float
  -> float

val prepend_opt : 'a option -> 'a list -> 'a list
val prepend_opt_map : f:('a -> 'b) -> 'a option -> 'b list -> 'b list
val fill_points : ?init:Path3.t -> n:int -> V3.t -> V3.t -> Path3.t
val idx_to_find : idx -> (int, 'a, 'cmp) Map.t -> 'a option
