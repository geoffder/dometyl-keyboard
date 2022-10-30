open! OCADml
open! OSCADml

type well =
  { radius : float
  ; angle : float
  ; tilt : float
  ; tilt_correction : xrot:float -> tilt:float -> float
  }

type fan =
  { radius : float
  ; angle : float
  ; tilt : float
  }

type curve =
  { well : well option
  ; fan : fan option
  }

type custom = int -> Key.t -> Key.t

type t =
  | Curve of curve
  | Custom of custom
  | PreTweak of custom * curve
  | PostTweak of curve * custom
  | Mix of (int -> t)

val well
  :  ?tilt:float
  -> ?tilt_correction:(xrot:float -> tilt:float -> float)
  -> radius:float
  -> float
  -> well

val fan : ?tilt:float -> radius:float -> float -> fan
val curve : ?well:well -> ?fan:fan -> unit -> t
val custom : custom -> t
val pre_tweak : ?well:well -> ?fan:fan -> custom -> t
val post_tweak : ?well:well -> ?fan:fan -> custom -> t
val place : ?well:well -> ?fan:fan -> centre_idx:float -> int -> Key.t -> Key.t
val apply : centre_idx:float -> t -> int -> Key.t -> Key.t
