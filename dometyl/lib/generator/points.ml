open Base
open Scad_ml

type t =
  { top_left : V3.t
  ; top_right : V3.t
  ; bot_left : V3.t
  ; bot_right : V3.t
  ; centre : V3.t
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

let to_cw_path t = [ t.top_left; t.top_right; t.bot_right; t.bot_left ]
let to_ccw_path t = [ t.bot_left; t.bot_right; t.top_right; t.top_left ]

let of_cw_path_exn = function
  | [ top_left; top_right; bot_right; bot_left ] ->
    { top_left
    ; top_right
    ; bot_left
    ; bot_right
    ; centre = V3.mean [ top_left; top_right; bot_left; bot_right ]
    }
  | _ -> failwith "Expect list of length 4, with corner points in clockwise order."

let of_cw_path l =
  try Ok (of_cw_path_exn l) with
  | Failure e -> Error e

let overlapping_bounds a b =
  let a = Path3.bbox (to_cw_path a)
  and b = Path3.bbox (to_cw_path b) in
  match V3.bbox_intersect a b with
  | Some inter ->
    let area_inter = V3.bbox_area inter
    and area_a = (a.max.x -. a.min.x) *. (a.max.y -. a.min.y)
    and area_b = (b.max.x -. b.min.x) *. (b.max.y -. b.min.y) in
    area_inter /. (area_a +. area_b -. area_inter)
  | None -> 0.

let get t = function
  | `TL -> t.top_left
  | `TR -> t.top_right
  | `BL -> t.bot_left
  | `BR -> t.bot_right
  | `CN -> t.centre

let direction { top_left; top_right; _ } = V3.normalize V3.(top_left -@ top_right)

let mark t =
  let f p = Scad.translate p (Scad.sphere 0.5) in
  Scad.(
    union
      Color.
        [ color Black @@ f t.centre
        ; color Red @@ f t.top_right
        ; color Green @@ f t.top_left
        ; color Blue @@ f t.bot_right
        ; color Purple @@ f t.bot_left
        ])
