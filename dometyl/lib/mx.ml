open! Base
open! Scad_ml

module Hotswap = struct
  let ex = Model.import "../things/hotswap.stl" |> Model.color Color.FireBrick

  let cutout_ex =
    Model.import "../things/hotswap-cutout.stl" |> Model.color Color.DarkMagenta

  let combo_ex = Model.union [ ex; cutout_ex ]
end

let plate_clearance = 3.

let hole_config =
  let outer_w = 19.
  and inner_w = 13.9
  and inner_h = 13.8
  and thickness = 4.
  and cap_height = 6.25 in
  let clip hole =
    let clip =
      Model.rotate
        (Float.pi /. 2., 0., 0.)
        (Model.cube ~center:true (5., thickness -. 1.3, 0.5))
      |> Model.translate (0., inner_h /. 2., -1.3)
    in
    Model.difference hole [ clip; Model.mirror (0, 1, 0) clip ]
  in
  KeyHole.{ spec = Kind.Mx (); outer_w; inner_w; inner_h; thickness; clip; cap_height }
