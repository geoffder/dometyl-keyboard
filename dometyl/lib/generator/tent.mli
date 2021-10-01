open! Base
open! Scad_ml

type sink =
  | Pan of float
  | Counter

type fastener =
  | Magnet
  | Screw of
      { head_rad : float
      ; shaft_rad : float
      ; sink : sink
      ; height : float
      ; clearance : float
      }

val screw
  :  ?head_rad:float
  -> ?shaft_rad:float
  -> ?sink:sink
  -> ?height:float
  -> ?clearance:float
  -> unit
  -> fastener

type bump_loc =
  | Col of int * [ `N | `S ]
  | Thumb of [ `N of int | `E | `S of int | `W ]

val default_bumps : bump_loc list
val find_bump_wall : Walls.t -> bump_loc -> Wall.t option

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
  -> ?fastener:fastener
  -> ?foot_thickness:float
  -> ?foot_rad:float
  -> ?bumpon_rad:float
  -> ?bumpon_inset:float
  -> ?bump_locs:bump_loc list
  -> 'k Case.t
  -> Scad.t
