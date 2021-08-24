open! Base
open! Scad_ml

(* let teeth ~inner_h ~thickness hole =
 *   let block =
 *     Model.rotate
 *       (Float.pi /. 2., 0., 0.)
 *       (Model.cube ~center:true (5., thickness -. 1.3, 0.5))
 *   and y = inner_h /. 2. in
 *   let north = Model.translate (0., y, -1.3) block
 *   and south = Model.translate (0., -.y, -1.3) block in
 *   Model.difference hole [ north; south ] *)

let make_hole
    ?cap
    ?(outer_w = 18.)
    ?(outer_h = 17.)
    ?(inner_w = 13.8)
    ?(inner_h = 13.8)
    ?(thickness = 4.)
    ?(cap_height = 5.)
    ?(clearance = 3.)
    ()
  =
  KeyHole.(
    make
      ?cap
      { spec = Kind.Mx ()
      ; outer_w
      ; outer_h
      ; inner_w
      ; inner_h
      ; thickness (* ; clip = teeth ~inner_h ~thickness *)
      ; clip = Fn.id
      ; cap_height
      ; clearance
      })
