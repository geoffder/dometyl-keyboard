open! Base
open! Scad_ml

type t =
  { top_left : Vec3.t
  ; top_right : Vec3.t
  ; bot_left : Vec3.t
  ; bot_right : Vec3.t
  ; centre : Vec3.t
  }

type pos =
  [ `BL
  | `BR
  | `CN
  | `TL
  | `TR
  ]

val map : f:(Vec3.t -> Vec3.t) -> t -> t
val fold : f:('a -> Vec3.t -> 'a) -> init:'a -> t -> 'a

include Sigs.Transformable with type t := t

val quaternion : Quaternion.t -> t -> t
val quaternion_about_pt : Quaternion.t -> Vec3.t -> t -> t
val to_clockwise_list : t -> Vec3.t list
val of_clockwise_list_exn : Vec3.t list -> t
val of_clockwise_list : Vec3.t list -> (t, string) Result.t
val overlapping_bounds : t -> t -> bool
val get : t -> [< `BL | `BR | `CN | `TL | `TR ] -> Vec3.t
val mark : t -> Model.t
