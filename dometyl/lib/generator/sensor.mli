open! Base
open! Scad_ml

module ThroughHole : sig
  type config =
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

  type t =
    { config : config
    ; scad : Scad.d3
    }

  val default : config
  val default_print : config
  val of_config : config -> t
  val sink : ?z:float -> t -> Float.t -> Scad.d3
end
