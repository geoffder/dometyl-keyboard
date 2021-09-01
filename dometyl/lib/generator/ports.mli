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

val place_tray
  :  ?x_off:float
  -> ?y_off:float
  -> ?z_rot:float
  -> Walls.t
  -> Model.t
  -> Model.t

val carbonfet_stl : bool -> Model.t

val carbonfet_holder
  :  ?micro:bool
  -> ?x_off:float
  -> ?y_off:float
  -> ?z_rot:float
  -> Walls.t
  -> Model.t

val derek_reversible_stl : bool -> Model.t

val reversible_holder
  :  ?reset_button:bool
  -> ?x_off:float
  -> ?y_off:float
  -> ?z_rot:float
  -> Walls.t
  -> Model.t
