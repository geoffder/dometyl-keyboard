open! Base
open! Scad_ml

val bumpon
  :  outer_rad:float
  -> inner_rad:float
  -> thickness:float
  -> inset:float
  -> Points.t
  -> Model.t * Model.t

val make
  :  ?degrees:float
  -> ?z_offset:float
  -> ?screw_height:float
  -> ?outer_screw_rad:float
  -> ?inner_screw_rad:float
  -> ?foot_thickness:float
  -> ?foot_rad:float
  -> ?bumpon_rad:float
  -> ?bumpon_inset:float
  -> 'k Case.t
  -> Model.t
