open! Base
open! Scad_ml

type t =
  { top_left : Vec3.t
  ; top_right : Vec3.t
  ; bot_left : Vec3.t
  ; bot_right : Vec3.t
  ; centre : Vec3.t
  }
[@@deriving scad]

type pos =
  [ `BL
  | `BR
  | `CN
  | `TL
  | `TR
  ]

val map : f:(Vec3.t -> Vec3.t) -> t -> t
val fold : f:('a -> Vec3.t -> 'a) -> init:'a -> t -> 'a
val to_clockwise_list : t -> Vec3.t list
val of_clockwise_list_exn : Vec3.t list -> t
val of_clockwise_list : Vec3.t list -> (t, string) Result.t
val overlapping_bounds : t -> t -> float
val get : t -> [< `BL | `BR | `CN | `TL | `TR ] -> Vec3.t
val mark : t -> Scad.d3
