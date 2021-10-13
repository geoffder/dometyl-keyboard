open! Base
open! Scad_ml

module Config : sig
  type t =
    { leg_w : float
    ; leg_thickness : float
    ; leg_l : float
    ; leg_spacing : float
    ; leg_bend : float
    ; leg_z_offset : float
    ; merge_legs : bool
    ; body_w : float
    ; body_l : float
    ; body_thickness : float
    }

  val a3144 : t
  val a3144_print : t
end

type t =
  { config : Config.t
  ; scad : Scad.d3
  }

val make : Config.t -> t
val sink : t -> Float.t -> Scad.d3
