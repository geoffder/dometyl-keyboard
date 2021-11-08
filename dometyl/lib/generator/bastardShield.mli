open! Base
open! Scad_ml

type t =
  { scad : Scad.d3
  ; thickness : float
  ; screw_l : Vec3.t
  ; screw_r : Vec3.t
  }
[@@deriving scad]

val print_pcb : ?right_hand:bool -> float -> Scad.d3
val pcb : float -> Scad.d3
val make : ?inset_depth:float -> ?thickness:float -> unit -> t
val screws : t -> Scad.d3

val place :
  ?x_off:float -> ?y_off:float -> ?z_off:float -> ?z_rot:float -> Walls.t -> t -> t

val eyelets :
     ?width:float
  -> ?z_off:float
  -> ?eyelet_config:Eyelet.config
  -> Connect.t
  -> t
  -> Scad.d3

val cutter :
     ?eye_width:float
  -> ?eye_z_off:float
  -> ?eyelet_config:Eyelet.config
  -> ?x_off:float
  -> ?y_off:float
  -> ?z_off:float
  -> ?z_rot:float
  -> t
  -> walls:Walls.t
  -> connections:Connect.t
  -> Ports.t

val to_scad : ?show_screws:bool -> t -> Scad.d3
