open! Base
open! Scad_ml

(** The top-level type of this library. The {!Model.t} scad held within this type
    should generally represent the finished case, made up of the three major building
    blocks: {!Plate.t}, {!Walls.t}, and {!Connect.t}. *)
type 'k t =
  { scad : Model.t
  ; plate : 'k Plate.t
  ; walls : Walls.t
  ; connections : Connect.t
  }

(** Basic transformation functions. *)
include Sigs.Transformable' with type 'k t := 'k t

(** [make ~plate_welder ~wall_builder ~base_connector plate]

A high level helper used to contsruct the case from provided functions and a {!Plate.t}.
- [~plate_welder]: a function intended to generate a {!Model.t} that provides support
  between columns.
- [~wall_builder]: will use the provided {!Plate.t} to create {!Walls.t} from the ends and
  sides of the columns to the ground.
- [~base_connector]: connects the walls around the perimeter of the case, using various
  functions provided by {!module:Connect}. This function should return a {!Connect.t} that
  follows along the entire edge of the case in clockwise fashion without gaps. This way
  the resulting {!Case.t} will contain the necessary information to use {!module:Bottom}
  and {!module:Tent}. *)
val make
  :  plate_welder:('k Plate.t -> Model.t)
  -> wall_builder:('k Plate.t -> Walls.t)
  -> base_connector:(Walls.t -> Connect.t)
  -> 'k Plate.t
  -> 'k t

(** [to_scad ?show_caps ?show_cutouts]

Extract the contained {!Model.t}, optionally tacking on keycaps and case cutouts (such as
representations of hotswap sockets, that are used to provide clearance), stored
in {!KeyHole.t}. *)
val to_scad : ?show_caps:bool -> ?show_cutouts:bool -> 'k t -> Model.t
