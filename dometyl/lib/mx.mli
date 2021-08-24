open! Base
open! Scad_ml

module Hotswap : sig
  val holder_thickness : float
  val socket_z : float
  val make : inner_w:float -> inner_h:float -> [< `North | `South ] -> Model.t * Model.t
  val combo_ex : Model.t
end

val teeth : inner_h:float -> thickness:float -> Model.t -> Model.t

val make_hole
  :  ?cap:Model.t
  -> ?hotswap:[< `North | `South ]
  -> ?outer_w:float
  -> ?inner_w:float
  -> ?inner_h:float
  -> ?thickness:float
  -> ?cap_height:float
  -> ?clearance:float
  -> unit
  -> KeyHole.Kind.mx KeyHole.t
