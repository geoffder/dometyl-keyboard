open! Base
open! Scad_ml

type bump_loc =
  | Thumb of Util.idx * Util.idx
  | Body of Util.idx * Util.idx
  | Point of V3.t

val default_bumps : bump_loc list
val locate_bump : 'k Plate.t -> bump_loc -> V3.t option

val make
  :  ?thickness:float
  -> ?fastener:Eyelet.fastener
  -> ?bumpon_rad:float
  -> ?bumpon_inset:float
  -> ?bump_locs:bump_loc list
  -> 'a Case.t
  -> Scad.d3
