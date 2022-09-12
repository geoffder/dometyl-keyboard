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

type custom = int -> Key.t -> Key.t

type t =
  | Curve of curve
  | Custom of custom
  | PreTweak of custom * curve
  | PostTweak of curve * custom

val spec : ?tilt:float -> radius:float -> float -> spec
val curve : ?well:spec -> ?fan:spec -> unit -> t
val custom : custom -> t
val pre_tweak : ?well:spec -> ?fan:spec -> custom -> t
val post_tweak : ?well:spec -> ?fan:spec -> custom -> t
val well_point : spec -> V3.t
val fan_point : spec -> V3.t
val well_theta : float -> spec -> int -> V3.t
val fan_theta : float -> spec -> int -> V3.t
val place : ?well:spec -> ?fan:spec -> centre_idx:float -> int -> Key.t -> Key.t
val apply : centre_idx:float -> t -> int -> Key.t -> Key.t
