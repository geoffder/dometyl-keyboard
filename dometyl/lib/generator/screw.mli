open! Base
open! Scad_ml

type hole =
  | Through
  | Inset of float

type config =
  { outer_rad : float
  ; inner_rad : float
  ; thickness : float
  ; hole : hole
  }

type t =
  { scad : Model.t
  ; centre : Vec3.t
  ; config : config
  }

val translate : Vec3.t -> t -> t
val rotate : Vec3.t -> t -> t
val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t
val default_config : config
val m4_config : config
val bumpon_config : config
val make : normal:Vec3.t -> config -> Vec3.t -> Vec3.t -> t
val to_scad : t -> Model.t
