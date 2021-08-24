open! Base
open! Scad_ml

type 'k t =
  { scad : Model.t
  ; plate : 'k Plate.t
  ; walls : Walls.t
  ; connections : Connect.t
  }

val translate : Vec3.t -> 'k t -> 'k t
val rotate : Vec3.t -> 'k t -> 'k t
val rotate_about_pt : Vec3.t -> Vec3.t -> 'k t -> 'k t

val make
  :  plate_welder:('k Plate.t -> Model.t)
  -> wall_builder:('k Plate.t -> Walls.t)
  -> base_connector:(Walls.t -> Connect.t)
  -> 'k Plate.t
  -> 'k t

val to_scad : ?show_caps:bool -> ?show_cutouts:bool -> 'k t -> Model.t
