open! Base
open! Scad_ml

module Hotswap : sig
  val make
    :  inner_w:float
    -> inner_h:float
    -> plate_thickness:float
    -> [< `North | `South ]
    -> Model.t * Model.t

  val example : ?alpha:float -> ?show_cutout:bool -> [ `North | `South ] -> Model.t
end

val teeth : inner_h:float -> thickness:float -> Model.t -> Model.t

val make_hole
  :  ?cap:Model.t
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
