open Base
open Scad_ml

type matrix =
  { x : Core.pos_t
  ; y : Core.pos_t
  ; z : Core.pos_t
  }

type euler = float * float * float

let align a b =
  if Math.(
       equal a b || equal a (negate b) || equal a (0., 0., 0.) || equal b (0., 0., 0.))
  then None
  else (
    let x = Math.norm a
    and z = Math.(norm (cross a b)) in
    Some { x; y = Math.(norm (cross z x)); z } )

let align_exn a b = Option.value_exn (align a b)

let euler_of_matrix m =
  let open Util in
  let x = Float.atan2 (get_z m.y) (get_z m.z) in
  let y =
    Float.atan2
      (-1. *. get_z m.x)
      (Float.sqrt ((get_z m.y *. get_z m.y) +. (get_z m.z *. get_z m.z)))
  in
  let z = Float.atan2 (get_y m.x) (get_x m.x) in
  x, y, z
