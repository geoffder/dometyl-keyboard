open! Base
open! Scad_ml

val make
  :  ?length:float
  -> ?jack_radius:float
  -> ?jack_width:float
  -> ?usb_height:float
  -> ?usb_width:float
  -> ?board_width:float
  -> ?board_thickness:float
  -> ?dist:float
  -> ?x_off:float
  -> ?z_off:float
  -> Walls.t
  -> Model.t

(* val carbonfet_elite : ?x_off:float -> ?y_off:float -> Walls.t -> Model.t
 * val carbonfet_micro : ?x_off:float -> ?y_off:float -> Walls.t -> Model.t *)
val carbonfet_holder : ?micro:bool -> ?x_off:float -> ?y_off:float -> Walls.t -> Model.t
