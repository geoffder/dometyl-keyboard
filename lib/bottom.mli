open OCADml
open OSCADml

type bump_loc

(** [thumb ?loc col row]

    Compute a bumpon location under the thumb key at [col] and [row] with a
    relative position of [loc] in a coordinate space where the origin of
    the key is [(v2 0.5 0.5)]. *)
val thumb : ?loc:v2 -> Idx.t -> Idx.t -> bump_loc

(** [body ?loc col row]

    Compute a bumpon location under the body key at [col] and [row] with a
    relative position of [loc] in a coordinate space where the origin of
    the key is [(v2 0.5 0.5)]. *)
val body : ?loc:v2 -> Idx.t -> Idx.t -> bump_loc

(** [point p]

    Specify a a bumpon location at the absolute xy coordinate [p]. *)
val point : v2 -> bump_loc

(** A list of default relative (placed under keyholes) bumpon locations *)
val default_bumps : bump_loc list

(** [locate_bump plate loc]

    Get the xy coordinate specified by [loc], if it is a legal position. *)
val locate_bump : Plate.t -> bump_loc -> V2.t option

(** [make case]

    Generate a base plate to fit [case].

    - [thickness] sets the thickness of the plate in mm (default [= 1.65])
    - [bumpon_rad] sets the bumpon inset radius (default [= 5.5])
    - [bumpon_inset] sets how deep the bumpon insets should be (default [= 0.8])
    - if provided, [fastener] overrides the fastener information carried by the
      case *)
val make
  :  ?thickness:float
  -> ?fastener:Eyelet.fastener
  -> ?bumpon_rad:float
  -> ?bumpon_inset:float
  -> ?bump_locs:bump_loc list
  -> Case.t
  -> Scad.d3
