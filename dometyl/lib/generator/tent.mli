open! Base
open! Scad_ml

type bump_loc =
  | Col of Util.idx * [ `N | `S ]
  | Thumb of [ `N of Util.idx | `E | `S of Util.idx | `W ]

val default_bumps : bump_loc list
val find_bump_wall : Walls.t -> bump_loc -> Wall.t option

val bumpon
  :  ?n_steps:int
  -> outer_rad:float
  -> inner_rad:float
  -> thickness:float
  -> inset:float
  -> Points.t
  -> Scad.d3 * Scad.d3

val make
  :  ?degrees:float
  -> ?z_offset:float
  -> ?fastener:Eyelet.fastener
  -> ?foot_thickness:float
  -> ?foot_rad:float
  -> ?bumpon_rad:float
  -> ?bumpon_inset:float
  -> ?bump_locs:bump_loc list
  -> 'k Case.t
  -> Scad.d3
