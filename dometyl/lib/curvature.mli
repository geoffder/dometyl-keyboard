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

type 'k custom = int -> 'k KeyHole.t -> 'k KeyHole.t

type 'k t =
  | Curve of curve
  | Custom of 'k custom
  | PreTweak of 'k custom * curve
  | PostTweak of curve * 'k custom

val spec : ?tilt:float -> radius:float -> float -> spec
val curve : ?well:spec -> ?fan:spec -> unit -> 'k t
val custom : 'k custom -> 'k t
val pre_tweak : ?well:spec -> ?fan:spec -> 'k custom -> 'k t
val post_tweak : ?well:spec -> ?fan:spec -> 'k custom -> 'k t
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
