open! Base
open! Scad_ml

val make
  :  ?thickness:float
  -> ?fastener:Screw.fastener
  -> ?bumpon_rad:float
  -> ?bumpon_inset:float
  -> 'a Case.t
  -> Scad.t
