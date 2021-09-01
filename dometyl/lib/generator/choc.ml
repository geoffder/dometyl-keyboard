open! Base
open! Scad_ml

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
      ; clip = Fn.id
      ; cap_height
      ; clearance
      })
