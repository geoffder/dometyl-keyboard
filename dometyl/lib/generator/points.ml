open Base
open Scad_ml

type t =
  { top_left : Vec3.t
  ; top_right : Vec3.t
  ; bot_left : Vec3.t
  ; bot_right : Vec3.t
  ; centre : Vec3.t
  }
[@@deriving scad]

type pos =
  [ `TL
  | `TR
  | `BL
  | `BR
  | `CN
  ]

let map ~f t =
  { top_left = f t.top_left
  ; top_right = f t.top_right
  ; bot_left = f t.bot_left
  ; bot_right = f t.bot_right
  ; centre = f t.centre
  }

let fold ~f ~init t =
  let flipped = Fn.flip f in
  f init t.top_left |> flipped t.top_right |> flipped t.bot_left |> flipped t.bot_right

let to_clockwise_list t = [ t.top_left; t.top_right; t.bot_right; t.bot_left ]

let of_clockwise_list_exn = function
  | [ top_left; top_right; bot_right; bot_left ] ->
    { top_left
    ; top_right
    ; bot_left
    ; bot_right
    ; centre = Vec3.mean [ top_left; top_right; bot_left; bot_right ]
    }
  | _ -> failwith "Expect list of length 4, with corner points in clockwise order."

let of_clockwise_list l = try Ok (of_clockwise_list_exn l) with Failure e -> Error e

let overlapping_bounds a b =
  let a = Path3.bbox (to_clockwise_list a) and b = Path3.bbox (to_clockwise_list b) in
  (* intersection rectangle *)
  let top = Float.min a.max.y b.max.y
  and right = Float.min a.max.x b.max.x
  and bot = Float.max a.min.y b.min.y
  and left = Float.max a.min.x b.min.x in
  if not Float.(right < left || top < bot) then
    let intersect = (right -. left) *. (top -. bot)
    and area_a = (a.max.x -. a.min.x) *. (a.max.y -. a.min.y)
    and area_b = (b.max.x -. b.min.x) *. (b.max.y -. b.min.y) in
    intersect /. (area_a +. area_b -. intersect)
  else 0.

let get t = function
  | `TL -> t.top_left
  | `TR -> t.top_right
  | `BL -> t.bot_left
  | `BR -> t.bot_right
  | `CN -> t.centre

let mark t =
  let f p = Scad.cube ~center:true (v3 1. 1. 1.) |> Scad.translate p in
  Scad.union [ f t.centre; f t.top_right; f t.top_left; f t.bot_right; f t.bot_left ]
