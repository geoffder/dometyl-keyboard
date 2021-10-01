open! Base
open! Scad_ml

type hole =
  | Through
  | Inset of float

type placement =
  | Normal of Vec3.t
  | Point of Vec3.t

type config =
  { outer_rad : float
  ; inner_rad : float
  ; thickness : float
  ; hole : hole
  }

type t =
  { scad : Scad.t
  ; cut : Scad.t option
  ; centre : Vec3.t
  ; config : config
  }

include Sigs.Transformable with type t := t

val default_config : config
val m4_config : config
val bumpon_config : config
val magnet_6x3_config : config
val make : ?n_steps:int -> placement:placement -> config -> Vec3.t -> Vec3.t -> t
val to_scad : t -> Scad.t
