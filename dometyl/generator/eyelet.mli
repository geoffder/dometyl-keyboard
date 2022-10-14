open OCADml
open OSCADml

type hole =
  | Through
  | Inset of
      { depth : float
      ; punch : [ `Rel of float | `Abs of float ] option
      }

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
      ; clearance : float option
      }

type wall_loc =
  | Body of [ `N | `E | `S | `W ] * Idx.t
  | Thumb of [ `N | `E | `S | `W ] * Idx.t

type placement =
  | Normal of V3.t
  | Point of V3.t

type config =
  { outer_rad : float
  ; inner_rad : float
  ; thickness : float
  ; hole : hole
  }

type t =
  { scad : Scad.d3
  ; cut : Scad.d3 option
  ; centre : V3.t
  ; config : config
  }
[@@deriving cad]

val inset : ?punch:[ `Rel of float | `Abs of float ] -> float -> hole

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
val make : ?fn:int -> ?fillet:float -> placement:placement -> config -> Path3.t -> t

val place
  :  ?fn:int
  -> ?fillet:float
  -> ?width:float
  -> ?bury:float
  -> ?config:config
  -> inline:Path3.t
  -> outline:Path3.t
  -> [ `Loc of v3 | `Reloc of v3 | `U of float ]
  -> t

val default_wall_locs : wall_loc list
val wall_locations : walls:Walls.t -> wall_loc list -> [> `Reloc of v3 ] list
val to_scad : t -> Scad.d3
val apply : t -> Scad.d3 -> Scad.d3
