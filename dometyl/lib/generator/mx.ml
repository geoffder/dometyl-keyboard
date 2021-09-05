open! Base
open! Scad_ml

module Hotswap = struct
  let make ~inner_w ~inner_h ~plate_thickness facing =
    let holder_thickness = 3.
    and hole_depth = 5. in
    let shallowness = hole_depth -. plate_thickness in
    let socket_z = -2.6 -. ((hole_depth +. shallowness) /. 2.)
    (* hotswap socket cutout position *)
    and w = inner_w +. 3.
    and h = inner_h +. 3.
    and z = (holder_thickness +. hole_depth +. shallowness) /. -2.
    (* the bottom of the hole.  *)
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
    ( ( if Float.(shallowness > 0.)
      then (
        let spacer =
          Model.difference
            (Model.square ~center:true (w, h))
            [ Model.square ~center:true (inner_w, inner_h) ]
          |> Model.linear_extrude ~height:shallowness
          |> Model.translate (0., 0., (plate_thickness /. -2.) -. shallowness)
        in
        Model.union [ hotswap; spacer ] )
      else hotswap )
    , cutout )

  let example ?alpha ?(show_cutout = false) facing =
    let swap, cut = make ~inner_w:13.9 ~inner_h:13.8 ~plate_thickness:5. facing in
    let swap = Model.color ?alpha Color.FireBrick swap in
    if show_cutout
    then Model.union [ swap; Model.color ?alpha Color.DarkMagenta cut ]
    else swap
end

let teeth ~inner_h ~thickness hole =
  let block = Model.cube ~center:true (5., 0.51, thickness -. 1.3)
  and y = (inner_h /. 2.) +. 0.25 in
  let north = Model.translate (0., y, -1.3) block
  and south = Model.translate (0., -.y, -1.3) block in
  Model.difference hole [ north; south ]

let make_hole
    ?cap
    ?hotswap
    ?(outer_w = 19.)
    ?(outer_h = 19.)
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
      let swap, cutout =
        Hotswap.make ~inner_w ~inner_h ~plate_thickness:thickness facing
      in
      let clip hole = Model.union [ teeth ~inner_h ~thickness hole; swap ] in
      clearance +. 3., clip, Some cutout
    | None        -> clearance, teeth ~inner_h ~thickness, None
  in
  KeyHole.(
    make
      ?cap
      ?cutout
      { spec = Kind.Mx ()
      ; outer_w
      ; outer_h
      ; inner_w
      ; inner_h
      ; thickness
      ; clip
      ; cap_height
      ; clearance
      })
