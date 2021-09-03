open! Base
open! Scad_ml

type t =
  { scad : Model.t
  ; thickness : float
  ; screw_l : Vec3.t
  ; screw_r : Vec3.t
  }

include Sigs.Transformable with type t := t

val pcb : float -> Model.t
val make : ?inset_depth:float -> ?thickness:float -> unit -> t
val screws : t -> Model.t

val place
  :  ?x_off:float
  -> ?y_off:float
  -> ?z_off:float
  -> ?z_rot:float
  -> Walls.t
  -> t
  -> t

val eyelets
  :  ?width:float
  -> ?z_off:float
  -> ?screw_config:Screw.config
  -> Connect.t
  -> t
  -> Model.t

val cutter
  :  ?eye_width:float
  -> ?eye_z_off:float
  -> ?screw_config:Screw.config
  -> ?x_off:float
  -> ?y_off:float
  -> ?z_off:float
  -> ?z_rot:float
  -> t
  -> walls:Walls.t
  -> connections:Connect.t
  -> Ports.t

val to_scad : ?show_screws:bool -> t -> Model.t
