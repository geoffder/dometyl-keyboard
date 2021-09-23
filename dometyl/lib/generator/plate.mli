(** Switch plate generation. *)

open! Base
open! Scad_ml

(** Provides per-column lookup functions to be used by {!val:make} for placement
    of {!KeyHole.t}s to form {!Column.t}s, and for arrangment and orientation
    of the generated columns to form the plate. *)
module Lookups : sig
  (** Record containing the column generation and placement functions. When each column is
      being generated (and placed) its index will be given to the lookup functions here
      to get the relevant settings. *)
  type 'k t =
    { offset : int -> Vec3.t
          (** Translation of columns following default spacing and tenting. *)
    ; curve : int -> 'k Curvature.t
          (** Distribution of {!KeyHole.t}s to form the columns *)
    ; swing : int -> float
          (** Rotation around columnar y-axis, which can emulate row curvature,
              adding to or subtracting from rotation due to tenting. *)
    ; splay : int -> float
          (** Rotation around columnar z-axis (origin of [centre_row] key). Applying splay
              will always have to go hand in hand with adjustments to {!offset} to end up
              with something sensible. *)
    ; rows : int -> int (** Number of keys to distribute for the given column. *)
    }

  val default_offset : int -> Vec3.t
  val default_curve : int -> 'k Curvature.t
  val default_swing : int -> float
  val default_splay : int -> float
  val default_rows : int -> int

  (** [make ?offset ?curve ?swing ?splay ()]

      Optionally provide each of the functions to overide the defaults. See {!type:t}
      for overview of what each one is used for. *)
  val make
    :  ?offset:(int -> Vec3.t)
    -> ?curve:(int -> 'k Curvature.t)
    -> ?swing:(int -> float)
    -> ?splay:(int -> float)
    -> ?rows:(int -> int)
    -> unit
    -> 'k t
end

(** Non-function parameters used for the construction of the plate. *)
type 'k config =
  { n_rows : int -> int
  ; centre_row : int
  ; n_cols : int
  ; centre_col : int
  ; spacing : float
  ; tent : float
  ; thumb_offset : Vec3.t
  ; thumb_angle : Vec3.t
  }

(** The plate is constructed from a main body of {!Columns.t} and a thumb cluster,
    modelled as a separate {!Column.t}. As with lower types, the config as well as the
    aggregate {!Scad.t} are carried around for convenient access. *)
type 'k t =
  { config : 'k config
  ; scad : Scad.t
  ; columns : 'k Columns.t
  ; thumb : 'k Column.t
  }

(** Basic transformation functions, applied to all relevant non-config contents. *)
include Sigs.Transformable' with type 'k t := 'k t

(** [make_thumb ~n_keys ~centre_idx ~curve ~rotate_clips ~caps keyhole]

Factored out helper function for creation of the thumb cluster, using the
distribution specification [curve] to place [n_keys] [keyhole]s, around the
[centre_idx] key, with caps according to the lookup [caps]. [rotate_clips] flags
whether to rotate the switch-clips / sockets / etc by 90 degrees. Non-rotated,
the clips will be however they are on a usual column(e.g. oriented top to
bottom). You may want to set this according to orientation you would like to
have your caps in, since Mx stems are not necessarily radially symmetric. *)
val make_thumb
  :  n_keys:int
  -> centre_idx:int
  -> curve:'k Curvature.t
  -> caps:(int -> Scad.t)
  -> rotate_clips:bool
  -> 'k KeyHole.t
  -> 'k Column.t

(** [make ?n_rows ?centre_row ?n_cols ?centre_col ?spacing ?tent ?n_thumb_keys ?thumb_centre
      ?thumb_curve ?rotate_thumb_clips ?thumb_offset ?thumb_angle ?lookups ?caps ?thum_caps
      keyhole]

Create a switch plate, with provided optional parameters and defaults. The default
parameters are used by the {!module:Splaytyl} and {!module:Closed} configurations,
and may or may not be suitable as a jumping off point for your own design. To that end,
other examples with divergent parameters are provided in the modules within the
[boards] sub-directory.
- [centre_row] is used by key distribution functions, namely {!Curvature.place}.
  Relevant to [thumb_curve] and {!Lookups.curve}.
- [n_cols] specifies number of columns on the main body of the switch plate
- [centre_col] specifies the column around which the others are positioned
  (typically 2, the middle finger) on the main body of the switch plate
- [spacing] gives the distance in mm between each column by default. Further adjustment
  can of course be done with {!Lookups.offset}.
- [tent] angle in radians that the entire plate (including thumb) will be y-rotated
- [n_thumb_keys], [thumb_centre], [thumb_curve] and [rotate_thumb_clips]: see {!val:make_thumb}
- [thumb_offset] directs the translation used for thumb cluster placement (starting point
  if zero origin).
- [thumb_angle] sets the euler (x -> y -> z) rotation used to orient the thumb before
  translating to its destination (rotation is about 0., oriented along x to start).
- [lookups] provides various per-column key/column-placement and orientation functions.
  See {!module:Lookups}.
- [caps] is a lookup from row number to a {!Scad.t} representing a keycap. This will be
  used to provide caps to {!KeyHole.t}s before they are distributed in {!Column.make}.
- [thumb_caps] same as [caps], but for the thumb. If not provided, this default to [caps].
- [keyhole] is the {!KeyHole.t} that will be distributed to make up the main plate and
  thumb cluster, carring all contained coordinate information and accesory {!Scad.t}s
  along with them to their new homes.
*)
val make
  :  ?centre_row:int
  -> ?n_cols:int
  -> ?centre_col:int
  -> ?spacing:float
  -> ?tent:float
  -> ?n_thumb_keys:int
  -> ?thumb_centre:int
  -> ?thumb_curve:'k Curvature.t
  -> ?rotate_thumb_clips:bool
  -> ?thumb_offset:Vec3.t
  -> ?thumb_angle:Vec3.t
  -> ?lookups:'k Lookups.t
  -> ?caps:(int -> Scad.t)
  -> ?thumb_caps:(int -> Scad.t)
  -> 'k KeyHole.t
  -> 'k t

(** [column_joins ?d1 ?d2 t]

    Returns a {!Scad.t} scad which joins together all columns of the main plate by
    hulling the {!KeyHole.Face.scad}s of the {!KeyHole.t}s together, for a closed
    switch-plate look (and sturdiness). Typically, a function such as this or
    {!val:skeleton_bridges} below will be used as as the [plate_welder] parameter
    to {!Case.make}. Optional params [in_d], [out_d1] and [out_d2] are passed on
    to {!Bridges.cols} to control how far the lower key's face is moved out, and
    the upper keys face is moved in before being hulled with its neighbours
    across the gap. This can be used to thicken the generated supports. *)
val column_joins : ?in_d:float -> ?out_d1:float -> ?out_d2:float -> 'k t -> Scad.t

(** [skeleton_bridges ?d1 ?d2 t]

    Returns a {!Scad.t} scad with "bridges" between the bottom row keys from
    index to middle, and the top keys for the remaining columns. Similar to the
    BastardKB skeletyl. This function, as well as {!val:column_joins} make use of
    functions found in the {!module:Bridges} module, if you are wanting to do
    something different, that would be a good place to get started. Optional
    params [in_d], [out_d1] and [out_d2] are passed on to {!Bridges.cols} to
    control how far the lower key's face is moved out, and the upper keys face
    is moved in before being hulled with its neighbours across the gap. This can
    be used to thicken the generated supports. *)
val skeleton_bridges : ?in_d:float -> ?out_d1:float -> ?out_d2:float -> 'k t -> Scad.t

(** [to_scad t]

    Extracts the {!Scad.t} from [t]. *)
val to_scad : 'k t -> Scad.t

(** [collect ~f t]

    Fold over all {!KeyHole.t}s contained within [t], extracting {!Scad.t}s with [f],
    and unioning them. Used for {!val:collect_caps} and {!val:collect_cutouts}. *)
val collect
  :  f:(key:int -> data:'k KeyHole.t -> Scad.t list -> Scad.t list)
  -> 'k t
  -> Scad.t

(** [collect_caps t]

    Return a {!Scad.t} representing the union between all caps carried by the
    {!KeyHole.t}s that make up the plate [t]. Used to design around keycap closeness,
    and lookout for collisions etc. *)
val collect_caps : 'k t -> Scad.t

(** [collect_cutout t]

    Return a {!Scad.t} representing the union between all {!KeyHole.cutout}s found
    in the plate [t]. This is used by {!Case.make} to perform the clearance cut from the
    {!Case.scad} model to make room for whatever the {!KeyHole.cutout} is modelling
    (e.g. hotswap sockets). Like {!val:collect_caps}, this could also be useful for
    visualization. *)
val collect_cutouts : 'k t -> Scad.t
