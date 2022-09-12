(** Switch plate generation. *)

open! Scad_ml

(** Provides per-column lookup functions to be used by {!val:make} for placement of
    {!Key.t}s to form {!Column.t}s, and for arrangment and orientation of the
    generated columns to form the plate. *)
module Lookups : sig
  (** Record containing the column generation and placement functions. When each column is
      being generated (and placed) its index will be given to the lookup functions here to
      get the relevant settings. *)
  type t =
    { offset : int -> V3.t
          (** Translation of columns following default spacing and tenting. *)
    ; curve : int -> Curvature.t (** Distribution of {!Key.t}s to form the columns *)
    ; swing : int -> float
          (** Rotation around columnar y-axis, which can emulate row curvature, adding to
              or subtracting from rotation due to tenting. *)
    ; splay : int -> float
          (** Rotation around columnar z-axis (origin of [centre_row] key). Applying splay
              will always have to go hand in hand with adjustments to {!offset} to end up
              with something sensible. *)
    ; rows : int -> int (** Number of keys to distribute for the given column. *)
    ; centre : int -> float
          (** Relative centre key index for the given column. Used by key distribution
              functions, namely {!Curvature.place}. Relevant to [thumb_curve] and
              {!Lookups.curve}. *)
    }

  val default_offset : int -> V3.t
  val default_curve : int -> Curvature.t
  val default_swing : int -> float
  val default_splay : int -> float
  val default_rows : int -> int
  val default_centre : int -> float

  (** [body ?offset ?curve ?swing ?splay ()]

      Optionally provide each of the functions to overide the defaults. See {!type:t} for
      overview of what each one is used for. *)
  val body
    :  ?offset:(int -> V3.t)
    -> ?curve:(int -> Curvature.t)
    -> ?swing:(int -> float)
    -> ?splay:(int -> float)
    -> ?rows:(int -> int)
    -> ?centre:(int -> float)
    -> unit
    -> t

  val thumb
    :  ?offset:(int -> V3.t)
    -> ?curve:(int -> Curvature.t)
    -> ?swing:(int -> float)
    -> ?splay:(int -> float)
    -> ?rows:(int -> int)
    -> ?centre:(int -> float)
    -> unit
    -> t
end

(** Non-function parameters used for the construction of the plate. *)
type config =
  { n_body_rows : int -> int
  ; body_centres : int -> float
  ; n_body_cols : int
  ; centre_col : int
  ; spacing : float
  ; tent : float
  ; n_thumb_rows : int -> int
  ; thumb_centres : int -> float
  ; n_thumb_cols : int
  ; thumb_offset : V3.t
  ; thumb_angle : V3.t
  }

(** The plate is constructed from a main body of {!Columns.t} and a thumb cluster,
    modelled as a separate {!Column.t}. As with lower types, the config as well as the
    aggregate {!Scad.d3} are carried around for convenient access. *)
type t =
  { config : config
  ; scad : Scad.d3
  ; body : Columns.t
  ; thumb : Columns.t
  }
[@@deriving scad]

(** [make ?n_rows ?centre_row ?n_cols ?centre_col ?spacing ?tent
    ?rotate_thumb_clips ?thumb_offset ?thumb_angle ?body_lookups ?thumb_lookups
    ?caps ?thum_caps keyhole]

    Create a switch plate, with provided optional parameters and defaults. The default
    parameters are used by the {!module:Splaytyl} and {!module:Closed} configurations, and
    may or may not be suitable as a jumping off point for your own design. To that end,
    other examples with divergent parameters are provided in the modules within the
    [boards] sub-directory.

    - [n_body_cols] specifies number of columns on the main body of the switch plate
    - [centre_col] specifies the column around which the others are positioned (typically
      2, the middle finger) on the main body of the switch plate
    - [spacing] gives the distance in mm between each column by default. Further
      adjustment can of course be done with {!Lookups.offset}.
    - [tent] angle in radians that the entire plate (including thumb) will be y-rotated
    - [thumb_offset] directs the translation used for thumb cluster placement (starting
      point if zero origin).
    - [thumb_angle] sets the euler (x -> y -> z) rotation used to orient the thumb before
      translating to its destination (rotation is about 0., oriented along x to start).
    - [body_lookups] and [thumb_lookups] provide various per-column key/column-placement
      and orientation functions for the construction of the body and thumb respectively.
      See {!module:Lookups}.
    - [caps] is a lookup from row number to a {!Scad.d3} representing a keycap. This will
      be used to provide caps to {!Key.t}s before they are distributed in
      {!Column.make}.
    - [thumb_caps] same as [caps], but for the thumb. If not provided, this default to
      [caps].
    - [keyhole] is the {!Key.t} that will be distributed to make up the main plate and
      thumb cluster, carring all contained coordinate information and accesory {!Scad.d3}s
      along with them to their new homes. *)
val make
  :  ?n_body_cols:int
  -> ?centre_col:int
  -> ?spacing:float
  -> ?tent:float
  -> ?n_thumb_cols:int
  -> ?rotate_thumb_clips:bool
  -> ?thumb_offset:V3.t
  -> ?thumb_angle:V3.t
  -> ?body_lookups:Lookups.t
  -> ?thumb_lookups:Lookups.t
  -> ?caps:(int -> Scad.d3)
  -> ?thumb_caps:(int -> Scad.d3)
  -> Key.t
  -> t

(** [column_joins ?in_d ?out_d1 ?out_d2 t]

    Returns a {!Scad.d3} scad which joins together all columns of the main plate by
    hulling the {!Key.Face.scad}s of the {!Key.t}s together, for a closed
    switch-plate look (and sturdiness). Typically, a function such as this or
    {!val:skeleton_bridges} below will be used as as the [plate_welder] parameter to
    {!Case.make}. Optional params [in_d], [out_d1] and [out_d2] are passed on to
    {!Bridges.cols} to control how far the lower key's face is moved out, and the upper
    keys face is moved in before being hulled with its neighbours across the gap. This can
    be used to thicken the generated supports. *)
val column_joins : ?in_d:float -> ?out_d1:float -> ?out_d2:float -> t -> Scad.d3

(** [skeleton_bridges ?in_d ?out_d1 ?out_d2 t]

    Returns a {!Scad.d3} scad with "bridges" between the bottom row keys from index to
    middle, and the top keys for the remaining columns. Similar to the BastardKB skeletyl.
    This function, as well as {!val:column_joins} make use of functions found in the
    {!module:Bridges} module, if you are wanting to do something different, that would be
    a good place to get started. Optional params [in_d], [out_d1] and [out_d2] are passed
    on to {!Bridges.cols} to control how far the lower key's face is moved out, and the
    upper keys face is moved in before being hulled with its neighbours across the gap.
    This can be used to thicken the generated supports. *)
val skeleton_bridges : ?in_d:float -> ?out_d1:float -> ?out_d2:float -> t -> Scad.d3

(** [to_scad t]

    Extracts the {!Scad.d3} from [t]. *)
val to_scad : t -> Scad.d3

(** [collect f t]

    Fold over all {!Key.t}s contained within [t], extracting {!Scad.d3}s with [f], and
    unioning them. Used for {!val:collect_caps} and {!val:collect_cutouts}. *)
val collect : (int -> Key.t -> Scad.d3 list -> Scad.d3 list) -> t -> Scad.d3

(** [collect_caps t]

    Return a {!Scad.d3} representing the union between all caps carried by the
    {!Key.t}s that make up the plate [t]. Used to design around keycap closeness, and
    lookout for collisions etc. *)
val collect_caps : t -> Scad.d3

(** [collect_cutout t]

    Return a {!Scad.d3} representing the union between all {!Key.cutout}s found in the
    plate [t]. This is used by {!Case.make} to perform the clearance cut from the
    {!Case.scad} model to make room for whatever the {!Key.cutout} is modelling (e.g.
    hotswap sockets). Like {!val:collect_caps}, this could also be useful for
    visualization. *)
val collect_cutouts : t -> Scad.d3
