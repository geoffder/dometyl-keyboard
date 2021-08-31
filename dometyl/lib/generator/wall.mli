open! Base
open! Scad_ml

module Steps : sig
  type t =
    [ `Flat of int
    | `PerZ of float
    ]

  val to_int : [< `Flat of int | `PerZ of float ] -> float -> int
end

module Edge : sig
  type t = float -> Vec3.t

  val translate : Vec3.t -> t -> t
  val rotate : Vec3.t -> t -> t
  val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t
  val point_at_z : ?max_iter:int -> ?tolerance:float -> t -> float -> Vec3.t
end

module Edges : sig
  type t =
    { top_left : Edge.t
    ; top_right : Edge.t
    ; bot_left : Edge.t
    ; bot_right : Edge.t
    }

  val map : f:(Edge.t -> Edge.t) -> t -> t
  val translate : Vec3.t -> t -> t
  val rotate : Vec3.t -> t -> t
  val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t
  val of_clockwise_list_exn : Edge.t list -> t
  val of_clockwise_list : Edge.t list -> (t, string) Result.t
  val get : t -> [< `BL | `BR | `TL | `TR ] -> Edge.t
end

type t =
  { scad : Model.t
  ; start : Points.t
  ; foot : Points.t
  ; edges : Edges.t
  ; screw : Screw.t option
  }

val translate : Vec3.t -> t -> t
val rotate : Vec3.t -> t -> t
val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t
val swing_face : ?step:float -> Vec3.t -> KeyHole.Face.t -> KeyHole.Face.t * Vec3.t

val poly_siding
  :  ?x_off:float
  -> ?y_off:float
  -> ?z_off:float
  -> ?clearance:float
  -> ?n_steps:[< `Flat of int | `PerZ of float > `Flat ]
  -> ?n_facets:int
  -> ?d1:float
  -> ?d2:float
  -> ?thickness:float
  -> ?screw_config:Screw.config
  -> [< `East | `North | `South | `West ]
  -> 'a KeyHole.t
  -> t

val column_drop
  :  ?z_off:float
  -> ?clearance:float
  -> ?d1:float
  -> ?d2:float
  -> ?thickness:float
  -> ?n_steps:[< `Flat of int | `PerZ of float > `Flat ]
  -> ?n_facets:int
  -> ?screw_config:Screw.config
  -> spacing:float
  -> columns:'k Columns.t
  -> [< `North | `South ]
  -> int
  -> t

val start_direction : t -> Vec3.t
val foot_direction : t -> Vec3.t
val to_scad : t -> Model.t
