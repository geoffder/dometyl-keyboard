open! Base
open! Scad_ml

let outer_w = 19.
let inner_w = 13.9
let inner_h = 13.8
let thickness = 4.
let cap_height = 6.25
let plate_clearance = 3.

module Hotswap = struct
  let ex = Model.import "../things/hotswap.stl" |> Model.color Color.FireBrick

  let cutout_ex =
    Model.import "../things/hotswap-cutout.stl" |> Model.color Color.DarkMagenta

  let combo_ex = Model.union [ ex; cutout_ex ]

  (* NOTE: for now, mimic val bindings from cloj, then simplify, and rename *)
  let mount_width = inner_w +. 3.
  let mount_height = inner_h +. 3.
  let holder_x = mount_width
  let holder_thickness = (holder_x -. inner_w) /. 2.
  let holder_y = inner_h +. (holder_thickness *. 2.)
  let swap_z = 3.
  let square_led_size = 6.
end

let hole_config =
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
