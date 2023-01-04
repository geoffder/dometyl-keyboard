open OCADml
open OSCADml

(** Shape(s) connecting walls together, and the points along the outside and inside of the
    foot. *)
type t =
  { scad : Scad.d3
  ; outline : Path3.t
  ; inline : Path3.t
  }
[@@deriving cad]

type config

(** [clockwise_union ts]

    Create a union of [ts]. They should be provided in clockwise
    order, such that the outline and inline of the resulting {!t} are continous and in the
    clockwise direction. *)
val clockwise_union : t list -> t

(** [outline_2d t]

    Retrieve the outline points of [t] and project them to 2d vectors. *)
val outline_2d : t -> Path2.t

(** [inline_2d t]

    Retrieve the inline points of [t] and project them to 2d vectors. *)
val inline_2d : t -> Path2.t

(** [full_join ?slices ?max_angle ?gap_fill ()]

    Configure a full joining (sealed top to bottom) of neighbouring walls.

    - [slices] sets the number of intervening profiles over which the face of
      the starting wall morphs into the destination wall (default [= 12])
    - if the angle between the heading of the start and destination walls is
      greater than [max_angle] (default [= 1.4]), pre-rotated faces will be
      projected out from each wall before the rest of the join is completed
    - if [gap_fill] is [true] (as is default), some effort will be made to
      close up any triangular gap there might be between the columns and the
      generated wall join *)
val full_join : ?slices:int -> ?max_angle:float -> ?gap_fill:bool -> unit -> config

(** [spline ()]

    Configure a base connection standing [height] tall between neighbouring
    walls that runs along a generated bezier spline path of [fn] points. The
    second and penultimate control points of which are offset [d] outward along
    the heading vectors of the input walls.

    - [size] corresponds to [~size] parameter of [Bezier3.of_path], setting the
      loosness of the bezier spline
    - [min_step_dist] constrains how close the profiles/slices of the sweep can
      be (discarded if they are not at least this distance off of the plane
      formed by the last). This is done to prevent (some of the possible)
      illegal self-intersecting meshes.
    - [fillet_d] and [fillet_h] describe a z-axis fillet, shrinking by the given
      height (absolute, or relative to [height]) over the specified distance
      (absolute, or relative to the length of the spline)
    - [corner] and [corner_fn] describe the roundover to apply to the top
      corners of the sweep profile
    - [max_edge_res] constrains the maximum resolution (defined as distance
      between points along the outline) of the sweep profile. Too high
      resolution can lead to "bunching" up of points during the sweep, especially
      with filleting that can introduce self-intersections into the mesh
    - [end_shrink] is a factor to scale down the first/final profiles of the
      sweep to help prevent poking through the walls (default [= 0.025])
    - [tight_threshold] and [tight_d] set an angle vs distance ratio (sharp
      angle between proximal walls) at which [d] is replaced by a (lower) value
      more likely to generate an unbroken mesh. Basically, too high of a [d]
      step outward from the walls at the beginning of the spline can lead to
      sweeps which can't help but overlap themselves *)
val spline
  :  ?height:float
  -> ?d:float
  -> ?size:float
  -> ?fn:int
  -> ?min_step_dist:float
  -> ?fillet_d:[ `Abs of float | `Rel of float ]
  -> ?fillet_h:[ `Abs of float | `Rel of float ]
  -> ?corner:Path3.Round.corner
  -> ?corner_fn:int
  -> ?max_edge_res:float
  -> ?end_shrink:float
  -> ?tight_threshold:float
  -> ?tight_d:float
  -> unit
  -> config

(** [manual walls]

    Generate a complete perimeter {!t} using lookups from wall index to
    {!config} for each of the sides, and the "links" between the body and the
    thumb cluster. *)
val manual
  :  ?west:(int -> config)
  -> ?north:(int -> config)
  -> ?south:(int -> config)
  -> ?east:(int -> config)
  -> ?east_link:config
  -> ?thumb_east:(int -> config)
  -> ?thumb_south:(int -> config)
  -> ?thumb_west:(int -> config)
  -> ?thumb_north:(int -> config)
  -> ?west_link:config
  -> Walls.t
  -> t

(** [skeleton walls]

    Generate a complete perimeter {!t} for a skeletal case with (reasonable?)
    defaults. See {!full_join} and {!spline} for configuration parameters
    relevant to the particular connection styles included.

   - [north_joins], [south_joins], and [close_thumb] can be used to indicate
    that full wall joins should be generated at particular connections around
    the case, rather than the spline connectors that just run along the ground *)
val skeleton
  :  ?height:float
  -> ?index_height:float
  -> ?thumb_height:float
  -> ?spline_d:float
  -> ?spline_size:float
  -> ?fn:int
  -> ?min_step_dist:float
  -> ?fillet_d:[ `Abs of float | `Rel of float ]
  -> ?fillet_h:[ `Abs of float | `Rel of float ]
  -> ?corner:Path3.Round.corner
  -> ?corner_fn:int
  -> ?max_edge_res:float
  -> ?end_shrink:float
  -> ?tight_threshold:float
  -> ?tight_spline_d:float
  -> ?join_slices:int
  -> ?max_join_angle:float
  -> ?gap_fill:bool
  -> ?east_link:config
  -> ?west_link:config
  -> ?north_joins:(int -> bool)
  -> ?south_joins:(int -> bool)
  -> ?close_thumb:bool
  -> Walls.t
  -> t

(** [closed walls]

    Generate a complete perimeter {!t} for a (mostly) closed case with (reasonable?)
    defaults. See {!full_join} and {!spline} for configuration parameters
    relevant to the particular connection styles included.

    This helper assumes that there are more walls in place than there would be
    for a skeletal case, as {!full_join} is at its most appropriate when the
    walls being joined are direct neighbours (especially around corners).

    Connections between the main body and the thumb cluster are not closed, as
    doing so would require more sealing logic prone to breaking, which goes
    against dometyls philosophy of allowing easy repositioning/re-orienting of
    the thumb cluster to suit the users hand. To adjust them, provide
    configurations with [west_link] and [east_link]. *)
val closed
  :  ?height:float
  -> ?spline_d:float
  -> ?spline_size:float
  -> ?fn:int
  -> ?min_step_dist:float
  -> ?fillet_d:[ `Abs of float | `Rel of float ]
  -> ?fillet_h:[ `Abs of float | `Rel of float ]
  -> ?corner:Path3.Round.corner
  -> ?corner_fn:int
  -> ?max_edge_res:float
  -> ?end_shrink:float
  -> ?tight_threshold:float
  -> ?tight_spline_d:float
  -> ?join_slices:int
  -> ?max_join_angle:float
  -> ?gap_fill:bool
  -> ?west_link:config
  -> ?east_link:config
  -> Walls.t
  -> t

val to_scad : t -> Scad.d3
