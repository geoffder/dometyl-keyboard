open! Scad_ml

(** Shape(s) connecting walls together, and the points along the outside and inside of the
    foot. *)
type t =
  { scad : Scad.d3
  ; outline : Path3.t
  ; inline : Path3.t
  }
[@@deriving scad]

type config

(** [clockwise_union ts] Create a union of [ts]. They should be provided in clockwise
    order, such that the outline and inline of the resulting {!t} are continous and in the
    clockwise direction. *)
val clockwise_union : t list -> t

(** [outline_2d t]

    Retrieve the outline points of [t] and project them to 2d vectors. *)
val outline_2d : t -> Path2.t

(** [inline_2d t]

    Retrieve the inline points of [t] and project them to 2d vectors. *)
val inline_2d : t -> Path2.t

val full_join
  :  ?slices:int
  -> ?max_angle:float
  -> ?gap_fill:[ `MinArea of float | `No ]
  -> unit
  -> config

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
  -> ?gap_fill:[ `MinArea of float | `No ]
  -> ?east_link:config
  -> ?west_link:config
  -> ?north_joins:(int -> bool)
  -> ?south_joins:(int -> bool)
  -> ?close_thumb:bool
  -> Walls.t
  -> t

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
  -> ?gap_fill:[ `MinArea of float | `No ]
  -> ?west_link:config
  -> ?east_link:config
  -> Walls.t
  -> t

val place_eyelet
  :  ?fn:int
  -> ?width:float
  -> ?bury:float
  -> ?eyelet_config:Eyelet.config
  -> ?relocate:bool
  -> inline:Path3.t
  -> outline:Path3.t
  -> v3
  -> Eyelet.t

val to_scad : t -> Scad.d3
