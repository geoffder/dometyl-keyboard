open! Base
open! Scad_ml

type spec =
  { radius : float
  ; angle : float
  ; tilt : float
  }

type curve =
  { well : spec option
  ; fan : spec option
  }

type 'k t =
  | Curve of curve
  | Custom of (int -> 'k KeyHole.t -> 'k KeyHole.t)

val spec : ?tilt:float -> radius:float -> float -> spec
val curve : ?well:spec -> ?fan:spec -> unit -> 'k t
val well_point : spec -> Vec3.t
val fan_point : spec -> Vec3.t
val well_theta : int -> spec -> int -> Vec3.t
val fan_theta : int -> spec -> int -> Vec3.t

val place
  :  ?well:spec
  -> ?fan:spec
  -> centre_idx:int
  -> int
  -> 'k KeyHole.t
  -> 'k KeyHole.t
