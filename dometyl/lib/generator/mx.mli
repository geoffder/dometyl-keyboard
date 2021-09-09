open! Base
open! Scad_ml

val kailh_socket : Scad.t

module Hotswap : sig
  val make
    :  inner_w:float
    -> inner_h:float
    -> plate_thickness:float
    -> [< `North | `South ]
    -> Scad.t * Scad.t

  val example : ?alpha:float -> ?show_cutout:bool -> [ `North | `South ] -> Scad.t
end

val teeth : inner_h:float -> thickness:float -> Scad.t -> Scad.t

val make_hole
  :  ?cap:Scad.t
  -> ?hotswap:[< `North | `South ]
  -> ?outer_w:float
  -> ?outer_h:float
  -> ?inner_w:float
  -> ?inner_h:float
  -> ?thickness:float
  -> ?cap_height:float
  -> ?clearance:float
  -> unit
  -> KeyHole.Kind.mx KeyHole.t
