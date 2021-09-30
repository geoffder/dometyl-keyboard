open! Base
open! Scad_ml

val bumpon
  :  ?n_steps:int
  -> outer_rad:float
  -> inner_rad:float
  -> thickness:float
  -> inset:float
  -> Points.t
  -> Scad.t * Scad.t

val make
  :  ?degrees:float
  -> ?z_offset:float
  -> ?screw_height:float
  -> ?outer_screw_rad:float
  -> ?inner_screw_rad:float
  -> ?screw_clearance:float
  -> ?foot_thickness:float
  -> ?foot_rad:float
  -> ?bumpon_rad:float
  -> ?bumpon_inset:float
  -> 'k Case.t
  -> Scad.t
