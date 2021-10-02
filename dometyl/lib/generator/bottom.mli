open! Base
open! Scad_ml

type idx =
  | First
  | Last
  | Idx of int

type bump_loc =
  | Thumb of idx
  | Col of idx * idx
  | Point of Vec3.t

val default_bumps : bump_loc list
val locate_bump : 'k Plate.t -> bump_loc -> Vec3.t option

val make
  :  ?thickness:float
  -> ?fastener:Eyelet.fastener
  -> ?bumpon_rad:float
  -> ?bumpon_inset:float
  -> ?bump_locs:bump_loc list
  -> 'a Case.t
  -> Scad.t
