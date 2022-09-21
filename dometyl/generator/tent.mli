open! Scad_ml

val default_bumps : float list

val make
  :  ?degrees:float
  -> ?fastener:Eyelet.fastener
  -> ?foot_thickness:float
  -> ?foot_rad:float
  -> ?bumpon_rad:float
  -> ?bumpon_inset:float
  -> ?bump_locs:float list
  -> Case.t
  -> Scad.d3
