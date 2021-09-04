open! Base
open! Scad_ml

(* NOTE: y value for block (3.) and its offset (3.6) are eyeballed from datasheet. Need
   to find a more comprehensive sheet, or measure myself when I get some. *)
let teeth ~inner_w ~thickness hole =
  let block = Model.cube ~center:true (0.51, 3., thickness -. 1.3)
  and x = (inner_w /. 2.) +. 0.25
  and y = 3.6 in
  let nw = Model.translate (-.x, y, -1.3) block
  and sw = Model.translate (-.x, -.y, -1.3) block
  and ne = Model.translate (x, y, -1.3) block
  and se = Model.translate (x, -.y, -1.3) block in
  Model.difference hole [ nw; sw; ne; se ]

let make_hole
    ?cap
    ?(outer_w = 19.)
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
      ; thickness
      ; clip = teeth ~inner_w ~thickness
      ; cap_height
      ; clearance
      })
