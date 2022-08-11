open! Base
open! Scad_ml

type t =
  { top_left : V3.t
  ; top_right : V3.t
  ; bot_left : V3.t
  ; bot_right : V3.t
  ; centre : V3.t
  }
[@@deriving scad]

type pos =
  [ `BL
  | `BR
  | `CN
  | `TL
  | `TR
  ]

val map : f:(V3.t -> V3.t) -> t -> t
val fold : f:('a -> V3.t -> 'a) -> init:'a -> t -> 'a
val to_clockwise_list : t -> V3.t list
val of_clockwise_list_exn : V3.t list -> t
val of_clockwise_list : V3.t list -> (t, string) Result.t
val overlapping_bounds : t -> t -> float
val get : t -> [< `BL | `BR | `CN | `TL | `TR ] -> V3.t
val mark : t -> Scad.d3
