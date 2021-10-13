open! Base
open! Scad_ml

type hole =
  | Through
  | Inset of float

type sink =
  | Pan of float
  | Counter

type fastener =
  | Magnet
  | Screw of
      { head_rad : float
      ; shaft_rad : float
      ; sink : sink
      ; height : float
      ; clearance : float
      }

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
[@@deriving scad]

val screw_fastener
  :  ?head_rad:float
  -> ?shaft_rad:float
  -> ?sink:sink
  -> ?height:float
  -> ?clearance:float
  -> unit
  -> fastener

val default_config : config
val m4_config : config
val bumpon_config : config
val magnet_6x3_config : config
val m4_countersunk_fastener : fastener
val make : ?n_steps:int -> placement:placement -> config -> Vec3.t -> Vec3.t -> t
val to_scad : t -> Scad.t
