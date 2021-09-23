open! Base
open! Scad_ml

val make
  :  ?thickness:float
  -> ?outer_screw_rad:float
  -> ?inner_screw_rad:float
  -> ?bumpon_rad:float
  -> ?bumpon_inset:float
  -> 'a Case.t
  -> Scad.t
