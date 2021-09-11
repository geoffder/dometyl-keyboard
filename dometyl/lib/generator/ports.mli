open! Base
open! Scad_ml

type t =
  { plus : Model.t option
  ; minus : Model.t option
  }

type cutter = walls:Walls.t -> connections:Connect.t -> t

val apply : t -> Model.t -> Model.t

val make
  :  ?length:float
  -> ?jack_radius:float
  -> ?jack_width:float
  -> ?usb_height:float
  -> ?usb_width:float
  -> ?usb_z_off:float
  -> ?board_width:float
  -> ?board_thickness:float
  -> ?dist:float
  -> ?x_off:float
  -> ?z_off:float
  -> unit
  -> cutter

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
  -> unit
  -> cutter

val derek_reversible_stl : bool -> Model.t

val reversible_holder
  :  ?reset_button:bool
  -> ?rail_w:float
  -> ?x_off:float
  -> ?y_off:float
  -> ?z_rot:float
  -> unit
  -> cutter
