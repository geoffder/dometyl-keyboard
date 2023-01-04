open! OCADml
open! OSCADml

(** Description of a keywell column (distributed along circle on the yz-plane) *)
type well

(** Description of a fanned column (distributed along circle on the xy-plane) *)
type fan

(** Column curvature to be applied to {!Key.t}'s via {!apply}. *)
type t

(** [well ?tilt ?tilt_correction ~radius angle]

    Describe a column of keys distributed in increments of [angle] radians
    along a circle of the given [radius] on the yz-plane. If provided [tilt]
    will apply a y-axis rotation to the key before the x-axis rotation of the
    well curvature, effectively {i tilting} inwards. This combination of
    rotations creates gaps between the keys, so by default, some counter
    z-axis rotation is applied. The default counter transformation can be
    overidden by [tilt_correction] (function from [xrot] and [tilt] radians to
    the radians to rotate around the z-axis). *)
val well
  :  ?tilt:float
  -> ?tilt_correction:(xrot:float -> tilt:float -> float)
  -> radius:float
  -> float
  -> well

(** [fan ?tilt ~radius angle]

    Describe a column of keys distributed in increments of [angle] radians
    along a circle of the given [radius] on the xy-plane. If provided [tilt]
    will apply a y-axis rotation to the key before the z-axis rotation of the
    fan curvature, effectively {i tilting} inwards. *)
val fan : ?tilt:float -> radius:float -> float -> fan

(** [curve ?well ?fan ()]

    Create a curvature {!type:t} with an (optional) combination of [well] and
    [fan] curvatures (see {!val:well} and {!val:fan}).  *)
val curve : ?well:well -> ?fan:fan -> unit -> t

(** [custom f]

    Create a curvature {!type:t} with an arbitrary function [f] that transforms a {!Key.t}
    to its desired placement along the column depending on the given row index. *)
val custom : (int -> Key.t -> Key.t) -> t

(** [pre_tweak ?well ?fan custom]

    Compose a curvature {!type:t} that will sequentialy apply and arbitrary
    function (see {!custom}) followed by standard well/fan {!val:curve}. *)
val pre_tweak : ?well:well -> ?fan:fan -> (int -> Key.t -> Key.t) -> t

(** [post_tweak ?well ?fan custom]

    Compose a curvature {!type:t} that will sequentialy apply a standardy
    well/fan {!val:curve} followed by an arbitrary function (see {!custom}).  *)
val post_tweak : ?well:well -> ?fan:fan -> (int -> Key.t -> Key.t) -> t

(** [mix f]

    Lift a function from row index to curvature {!type:t} into a {!type:t}.
    Useful for describing a column which follows a more/less extreme curvature
    on either side of the central row. *)
val mix : (int -> t) -> t

(** [apply ~centre_idx t idx key]

    Apply the column curvature [t] to the [key] at row [idx], around the
    [centre_idx] of the column (given as a float, such that it can lie between rows). *)
val apply : centre_idx:float -> t -> int -> Key.t -> Key.t
