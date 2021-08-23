open! Base
open! Scad_ml

module Hotswap = struct
  let holder_thickness = 3.
  let socket_z = -2.6 (* hotswap socket cutout position *)

  let make ~inner_w ~inner_h facing =
    let w = inner_w +. 3.
    and h = inner_h +. 3.
    and z = holder_thickness /. -2. (* the bottom of the hole.  *)
    and socket_thickness = holder_thickness +. 0.5 (* plus printing error *)
    and sign =
      match facing with
      | `North -> -1.
      | `South -> 1.
    in
    let cutout =
      let big =
        Model.square ~center:true (w +. 0.01, 4.3)
        |> Model.translate (0., 4.95 *. sign, 0.)
      and small =
        Model.square ~center:true (w /. 3. *. 1.95, 6.2)
        |> Model.translate (w /. -4.5 *. sign, 4. *. sign, 0.)
      in
      Model.union [ big; small ]
      |> Model.linear_extrude ~center:true ~height:socket_thickness
      |> Model.translate (0., 0., socket_z)
    in
    let hotswap =
      let access_cuts =
        let x = (w /. 2.) -. (w /. 8.04)
        and y = 7.4 in
        let cut = Model.square ~center:true (w /. 4., 2.01) in
        Model.union
          [ Model.translate (x, y *. sign, 0.) cut
          ; Model.translate (-.x, y *. sign, 0.) cut
          ]
        |> Model.linear_extrude ~center:true ~height:socket_thickness
        |> Model.translate (0., 0., socket_z)
      and led_cut =
        Model.square ~center:true (6., 6.) |> Model.translate (0., -6. *. sign, 0.)
      and holes =
        let main = Model.circle ~fn:30 2.05
        and pin = Model.circle ~fn:30 1.65
        and friction = Model.circle ~fn:30 0.975 in
        let plus = Model.translate (-3.81 *. sign, 2.54 *. sign, 0.) pin
        and minus = Model.translate (2.54 *. sign, 5.08 *. sign, 0.) pin
        and fric_left = Model.translate (-5., 0., 0.) friction
        and fric_right = Model.translate (5., 0., 0.) friction in
        Model.union [ main; plus; minus; fric_left; fric_right ]
      and holder =
        let slab = Model.square ~center:true (w, h)
        and tab =
          Model.square ~center:true (w /. 2., 1.)
          |> Model.translate (0., h /. 2. *. sign, 0.)
        in
        Model.union [ slab; tab ]
      in
      Model.difference holder [ led_cut; holes ]
      |> Model.linear_extrude ~center:true ~height:holder_thickness
      |> Model.translate (0., 0., z)
      |> Fn.flip Model.difference [ access_cuts; cutout ]
    in
    hotswap, cutout

  (* let ex = Model.import "../things/hotswap.stl" |> Model.color Color.FireBrick
   *
   * let cutout_ex =
   *   Model.import "../things/hotswap-cutout.stl" |> Model.color Color.DarkMagenta
   *
   * let combo_ex =
   *   Model.union [ ex; cutout_ex ] |> Model.translate (0., 0., thickness /. -2.) *)

  let combo_ex =
    let swap, cut = make ~inner_w:13.9 ~inner_h:13.8 `North in
    Model.union [ Model.color Color.FireBrick swap; Model.color Color.DarkMagenta cut ]
end

let teeth ~inner_h ~thickness hole =
  let clip =
    Model.rotate
      (Float.pi /. 2., 0., 0.)
      (Model.cube ~center:true (5., thickness -. 1.3, 0.5))
    |> Model.translate (0., inner_h /. 2., -1.3)
  in
  Model.difference hole [ clip; Model.mirror (0, 1, 0) clip ]

let make_hole
    ?cap
    ?hotswap
    ?(outer_w = 19.)
    ?(inner_w = 13.9)
    ?(inner_h = 13.8)
    ?(thickness = 4.)
    ?(cap_height = 6.25)
    ?(clearance = 3.)
    ()
  =
  let clearance, clip, cutout =
    match hotswap with
    | Some facing ->
      let swap, cutout = Hotswap.make ~inner_w ~inner_h facing in
      let clip hole =
        Model.union
          [ teeth ~inner_h ~thickness hole
          ; Model.translate (0., 0., thickness /. -2.) swap
          ]
      in
      clearance +. 3., clip, Some (Model.translate (0., 0., thickness /. -2.) cutout)
    | None        -> clearance, teeth ~inner_h ~thickness, None
  in
  KeyHole.(
    make
      ?cap
      ?cutout
      { spec = Kind.Mx ()
      ; outer_w
      ; inner_w
      ; inner_h
      ; thickness
      ; clip
      ; cap_height
      ; clearance
      })
