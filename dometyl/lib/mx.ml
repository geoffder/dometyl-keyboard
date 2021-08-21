open! Base
open! Scad_ml

let outer_w = 19.
let inner_w = 13.9
let inner_h = 13.8
let thickness = 4.
let cap_height = 6.25
let plate_clearance = 3.

module Hotswap = struct
  let w = inner_w +. 3.
  let h = inner_h +. 3.
  let holder_thickness = 3.
  let z = holder_thickness /. -2. (* the bottom of the hole.  *)

  (* thickness of kailh hotswap holder + some margin of printing error (0.5mm) *)
  let socket_thickness = holder_thickness +. 0.5
  let socket_z = -2.6

  let cutout facing =
    let sign =
      match facing with
      | `North -> -1.
      | `South -> 1.
    in
    let extra_w = 3.01
    and big_x = 0.
    and big_y = 4.95
    and small_x = w /. -4.5
    and small_y = 4. in
    Model.union
      [ Model.cube ~center:true (inner_w +. extra_w, 4.3, socket_thickness)
        |> Model.translate (big_x *. sign, big_y *. sign, socket_z)
      ; Model.cube ~center:true (w /. 3. *. 1.95, 6.2, socket_thickness)
        |> Model.translate (small_x *. sign, small_y *. sign, socket_z)
      ]

  let hotswap facing =
    let sign =
      match facing with
      | `North -> -1.
      | `South -> 1.
    in
    let access_cuts =
      let x = (w /. 2.) -. (w /. 8.04)
      and y = 7.4 in
      let block = Model.cube ~center:true (w /. 4., 2.01, socket_thickness) in
      Model.union
        [ Model.translate (x, y *. sign, socket_z) block
        ; Model.translate (-.x, y *. sign, socket_z) block
        ]
    and led_cut =
      Model.cube ~center:true (6., 6., holder_thickness +. 0.01)
      |> Model.translate (0., -6. *. sign, z)
    and holes =
      let cyl r = Model.cylinder ~center:true ~fn:30 r (thickness +. 0.01) in
      let main = cyl 2.05 |> Model.translate (0., 0., z)
      and pin = cyl 1.65
      and friction = cyl 0.975 in
      let plus = Model.translate (-3.81 *. sign, 2.54 *. sign, z) pin
      and minus = Model.translate (2.54 *. sign, 5.08 *. sign, z) pin
      and fric_left = Model.translate (-5., 0., z) friction
      and fric_right = Model.translate (5., 0., z) friction in
      Model.union [ main; plus; minus; fric_left; fric_right ]
    and holder =
      let slab =
        Model.cube ~center:true (w, h, holder_thickness) |> Model.translate (0., 0., z)
      and tab =
        Model.cube ~center:true (w /. 2., 1., holder_thickness)
        |> Model.translate (0., h /. -2., z)
      in
      Model.union [ slab; tab ]
    in
    Model.difference holder [ access_cuts; led_cut; holes; cutout facing ]

  let ex = Model.import "../things/hotswap.stl" |> Model.color Color.FireBrick

  let cutout_ex =
    Model.import "../things/hotswap-cutout.stl" |> Model.color Color.DarkMagenta

  (* let combo_ex =
   *   Model.union [ ex; cutout_ex ] |> Model.translate (0., 0., thickness /. -2.) *)

  let combo_ex =
    Model.union
      [ Model.color Color.FireBrick (hotswap `North)
      ; Model.color Color.DarkMagenta (cutout `North)
      ]
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
