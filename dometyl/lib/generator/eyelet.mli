open! Base
open! Scad_ml

type hole =
  | Through
  | Inset of float

type sink =
  | Pan of float
  | Counter

type fastener =
  | SameMagnet
  | Magnet of
      { rad : float
      ; thickness : float
      }
  | Screw of
      { head_rad : float
      ; shaft_rad : float
      ; sink : sink
      ; height : float
      ; clearance : float
      }

type placement =
  | Normal of Vec2.t
  | Point of Vec2.t

type config =
  { outer_rad : float
  ; inner_rad : float
  ; thickness : float
  ; hole : hole
  }

type t =
  { scad : Scad.d3
  ; cut : Scad.d3 option
  ; centre : Vec3.t
  ; config : config
  }
[@@deriving scad]

val screw_fastener :
     ?head_rad:float
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
val make : ?fn:int -> placement:placement -> config -> Vec2.t -> Vec2.t -> t
val to_scad : t -> Scad.d3
