open! Base
open! Scad_ml

val kailh_socket : Scad.d3
val switch : Scad.d3

module Hotswap : sig
  val make :
       inner_w:float
    -> inner_h:float
    -> outer_w:float
    -> outer_h:float
    -> plate_thickness:float
    -> [< `North | `South ]
    -> Scad.d3 * Scad.d3
end

val teeth : inner_w:float -> thickness:float -> Scad.d3 -> Scad.d3

val make_hole :
     ?render:bool
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
  -> unit
  -> KeyHole.Kind.key KeyHole.t

val example_assembly :
     ?show_cutout:bool
  -> ?show_switch:bool
  -> ?show_socket:bool
  -> ?show_cap:bool
  -> unit
  -> Scad.d3
