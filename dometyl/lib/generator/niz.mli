open! Base
open! Scad_ml

module Bottom : sig
  val x : float
  val y : float
  val z : float
  val bulge_thickness : float
  val bulge_length : float
  val bulge_height : float
  val ellipse_offset : float
  val ellipse_inset_x_rad : float
  val ellipse_inset_y_scale : float
  val corner_cut_rad : float
  val corner_cut_off : float
  val ellipse : Scad.d2
  val bulge : Scad.d3
  val cutter : Scad.d2
  val scad : Scad.d3
end

val hole_config : KeyHole.Kind.niz KeyHole.config

module Platform : sig
  type config =
    { w : float
    ; dome_w : float
    ; dome_waist : float
    ; dome_thickness : float
    ; base_thickness : float
    ; sensor_depth : float
    ; lug_height : float
    ; snap_clearance : float
    ; snap_len : float
    ; sensor_config : Sensor.Config.t
    }

  val default_config : config

  type t =
    { config : config
    ; wall_height : float
    ; scad : Scad.d3
    }

  val make : config -> t
end

val example_cross_section : Scad.d3
val new_hole_config : KeyHole.Kind.niz KeyHole.config
