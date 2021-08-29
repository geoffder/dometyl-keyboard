open! Base
open! Scad_ml
val make :
  ?length:float ->
  ?jack_radius:float ->
  ?jack_width:float ->
  ?usb_height:float ->
  ?usb_width:float ->
  ?board_width:float ->
  ?board_thickness:float ->
  ?dist:float ->
  ?x_off:float ->
  ?z_off:float -> Walls.t -> Model.t
