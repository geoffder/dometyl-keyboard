open OSCADml

type cutter = ?z:float -> float -> Scad.d3

module ThroughHole : sig
  val bulk_model
    :  ?leg_w:float
    -> ?leg_thickness:float
    -> ?leg_l:float
    -> ?leg_spacing:float
    -> ?leg_bend:float
    -> ?leg_z_offset:float
    -> ?merge_legs:bool
    -> ?body_w:float
    -> ?body_l:float
    -> ?body_thickness:float
    -> unit
    -> Scad.d3

  val bulk_cutout
    :  ?body_w:float
    -> ?body_l:float
    -> ?legs_w:float
    -> ?legs_l:float
    -> ?legs_z_offset:float
    -> ?slot_l:float
    -> unit
    -> ?z:float
    -> float
    -> Scad.d3

  val tape_cutout
    :  ?body_w:float
    -> ?body_l:float
    -> ?legs_w1:float
    -> ?legs_w2:float
    -> ?legs_l1:float
    -> ?legs_l2:float
    -> ?legs_z_offset:float
    -> ?slot_l:float
    -> unit
    -> ?z:float
    -> float
    -> Scad.d3
end
