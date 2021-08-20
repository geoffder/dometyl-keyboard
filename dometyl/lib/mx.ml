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
  let hotswap_x2 = holder_x /. 3. *. 1.95
  let hotswap_y1 = 4.3 (* first y-size of kailh hotswap holder *)

  let hotswap_y2 = 6.2 (* second y-size of kailh hotswap holder *)

  let hotswap_z = swap_z +. 0.5
  (* thickness of kailh hotswap holder + some margin of printing error (0.5mm) *)

  (* let hotswap_cutout_z_offset = -2.6
   * let hotswap_cutout_2_x_offset = -.(holder_x /. 4.5)
   * let hotswap_cutout_1_y_offset = 4.95
   * let hotswap_cutout_2_y_offset = 4.
   * let hotswap_case_cutout_x_extra = 3.01 *)

  let cutout facing =
    let sign =
      match facing with
      | `North -> -1.
      | `South -> 1.
    in
    let extra_w = 3.01
    and big_x = 0.
    and big_y = 4.95
    and small_x = holder_x /. -4.5
    and small_y = 4.
    and z = -2.6 in
    Model.union
      [ Model.cube ~center:true (inner_w +. extra_w, hotswap_y1, hotswap_z)
        |> Model.translate (big_x *. sign, big_y *. sign, z)
      ; Model.cube ~center:true (hotswap_x2, hotswap_y2, hotswap_z)
        |> Model.translate (small_x *. sign, small_y *. sign, z)
      ]

  let combo_ex = Model.union [ combo_ex; cutout `North ]
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
