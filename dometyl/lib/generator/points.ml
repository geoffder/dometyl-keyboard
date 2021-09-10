open Base
open Scad_ml

type t =
  { top_left : Vec3.t
  ; top_right : Vec3.t
  ; bot_left : Vec3.t
  ; bot_right : Vec3.t
  ; centre : Vec3.t
  }

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

let translate p = map ~f:(Vec3.add p)
let mirror ax = map ~f:(Vec3.mirror ax)
let rotate r = map ~f:(Vec3.rotate r)
let rotate_about_pt r p = map ~f:(Vec3.rotate_about_pt r p)
let quaternion q = map ~f:(Quaternion.rotate_vec3 q)
let quaternion_about_pt q p = map ~f:(Quaternion.rotate_vec3_about_pt q p)
let to_clockwise_list t = [ t.top_left; t.top_right; t.bot_right; t.bot_left ]

let of_clockwise_list_exn = function
  | [ top_left; top_right; bot_right; bot_left ] ->
    { top_left
    ; top_right
    ; bot_left
    ; bot_right
    ; centre =
        Vec3.(map (( *. ) 0.25) (top_left <+> top_right <+> bot_left <+> bot_right))
    }
  | _ -> failwith "Expect list of length 4, with corner points in clockwise order."

let of_clockwise_list l =
  try Ok (of_clockwise_list_exn l) with
  | Failure e -> Error e

let overlapping_bounds a b =
  let a_t, a_r, a_b, a_l = Util.bounding_box (to_clockwise_list a)
  and b_t, b_r, b_b, b_l = Util.bounding_box (to_clockwise_list b) in
  (* intersection rectangle *)
  let top = Float.min a_t b_t
  and right = Float.min a_r b_r
  and bot = Float.max a_b b_b
  and left = Float.max a_l b_l in
  if not Float.(right < left || top < bot)
  then (
    let intersect = (right -. left) *. (top -. bot)
    and area_a = (a_r -. a_l) *. (a_t -. a_b)
    and area_b = (b_r -. b_l) *. (b_t -. b_b) in
    intersect /. (area_a +. area_b -. intersect) )
  else 0.

let get t = function
  | `TL -> t.top_left
  | `TR -> t.top_right
  | `BL -> t.bot_left
  | `BR -> t.bot_right
  | `CN -> t.centre

let mark t =
  let f p = Model.cube ~center:true (1., 1., 1.) |> Model.translate p in
  Model.union [ f t.centre; f t.top_right; f t.top_left; f t.bot_right; f t.bot_left ]
