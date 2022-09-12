open! Scad_ml

val kailh_socket : Scad.d3

module Hotswap : sig
  val make
    :  inner_w:float
    -> inner_h:float
    -> plate_thickness:float
    -> [< `North | `South ]
    -> Scad.d3 * Scad.d3

  val example : ?alpha:float -> ?show_cutout:bool -> [ `North | `South ] -> Scad.d3
end

val teeth : inner_h:float -> thickness:float -> Scad.d3 -> Scad.d3

val make_hole
  :  ?render:bool
  -> ?cap:Scad.d3
  -> ?hotswap:[< `North | `South ]
  -> ?outer_w:float
  -> ?outer_h:float
  -> ?inner_w:float
  -> ?inner_h:float
  -> ?thickness:float
  -> ?cap_height:float
  -> ?cap_cutout_height:float option
  -> ?clearance:float
  -> ?corner:Path3.Round.corner
  -> ?fn:int
  -> unit
  -> Key.t
