open! Base
open! Scad_ml

(* Taken from here: https://grabcad.com/library/kailh-hotswap-mx-1
   For rough visualization. It does not seem to line up with the holes, even
   when holes are positioned more exactly according to specifications. *)
let kailh_socket =
  Scad.import_3d "../things/switches/mx_hotswap_socket.stl"
  |> Scad.translate (0.65, -4.8, -5.15)
  |> Scad.color ~alpha:0.4 Color.Silver

module Hotswap = struct
  let make ~inner_w ~inner_h ~plate_thickness facing =
    let holder_thickness = 3. and hole_depth = 5. in
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
        Scad.square ~center:true (w +. 0.01, 4.3) |> Scad.translate (0., 4.95 *. sign, 0.)
      and small =
        Scad.square ~center:true (w /. 3. *. 1.95, 6.2)
        |> Scad.translate (w /. -4.5 *. sign, 4. *. sign, 0.)
      in
      Scad.union [ big; small ]
      |> Scad.linear_extrude ~center:true ~height:socket_thickness
      |> Scad.translate (0., 0., socket_z)
    in
    let hotswap =
      let access_cuts =
        let x = (w /. 2.) -. (w /. 8.04) and y = h /. 2. in
        let cut = Scad.square ~center:true (w /. 4., 5.) in
        Scad.union
          [ Scad.translate (x, y *. sign, 0.) cut
          ; Scad.translate (-.x, y *. sign, 0.) cut
          ]
        |> Scad.linear_extrude ~center:true ~height:socket_thickness
        |> Scad.translate (0., 0., socket_z)
      and led_cut =
        Scad.square ~center:true (6., 6.) |> Scad.translate (0., -6. *. sign, 0.)
      and holes =
        let main = Scad.circle ~fn:30 2.05
        and pin = Scad.circle ~fn:30 1.65
        and friction = Scad.circle ~fn:30 0.975 in
        let plus = Scad.translate (-3.81 *. sign, 2.54 *. sign, 0.) pin
        and minus = Scad.translate (2.54 *. sign, 5.08 *. sign, 0.) pin
        and fric_left = Scad.translate (-5., 0., 0.) friction
        and fric_right = Scad.translate (5., 0., 0.) friction in
        Scad.union [ main; plus; minus; fric_left; fric_right ]
      and holder =
        let slab = Scad.square ~center:true (w, h)
        and tab =
          Scad.square ~center:true (w /. 2., 1.)
          |> Scad.translate (0., h /. 2. *. sign, 0.)
        in
        Scad.union [ slab; tab ]
      in
      Scad.difference holder [ led_cut; holes ]
      |> Scad.linear_extrude ~center:true ~height:holder_thickness
      |> Scad.translate (0., 0., z)
      |> Fn.flip Scad.difference [ access_cuts; cutout ]
    in
    ( ( if Float.(shallowness > 0.) then
        let spacer =
          Scad.difference
            (Scad.square ~center:true (w, h))
            [ Scad.square ~center:true (inner_w, inner_h) ]
          |> Scad.linear_extrude ~height:shallowness
          |> Scad.translate (0., 0., (plate_thickness /. -2.) -. shallowness)
        in
        Scad.union [ hotswap; spacer ]
      else hotswap )
    , cutout )

  let example ?alpha ?(show_cutout = false) facing =
    let swap, cut = make ~inner_w:13.9 ~inner_h:13.8 ~plate_thickness:5. facing in
    let swap = Scad.color ?alpha Color.FireBrick swap in
    if show_cutout then Scad.union [ swap; Scad.color ?alpha Color.DarkMagenta cut ]
    else swap
end

let teeth ~inner_h ~thickness hole =
  let block = Scad.cube ~center:true (5., 0.51, thickness -. 1.3)
  and y = (inner_h /. 2.) +. 0.25 in
  let north = Scad.translate (0., y, -1.3) block
  and south = Scad.translate (0., -.y, -1.3) block in
  Scad.difference hole [ north; south ]

let make_hole
    ?render
    ?cap
    ?hotswap
    ?(outer_w = 19.)
    ?(outer_h = 19.)
    ?(inner_w = 13.95)
    ?(inner_h = 13.95)
    ?(thickness = 4.)
    ?(cap_height = 6.25)
    ?(cap_cutout_height = Some 1.5)
    ?(clearance = 3.)
    () =
  let clearance, clip, cutout =
    match hotswap with
    | Some facing ->
      let swap, cutout =
        Hotswap.make ~inner_w ~inner_h ~plate_thickness:thickness facing
      in
      let clip hole = Scad.union [ teeth ~inner_h ~thickness hole; swap ] in
      clearance +. 3., clip, Some cutout
    | None        -> clearance, teeth ~inner_h ~thickness, None
  and cap_cutout =
    Option.map
      ~f:(fun h ->
        Scad.translate
          (0., 0., 2.5 +. h +. (thickness /. 2.))
          (Scad.cube ~center:true (20., 20., 5.)) )
      cap_cutout_height
  in
  KeyHole.(
    make
      ?render
      ?cap
      ?cutout:(Option.merge ~f:(fun a b -> Scad.union [ a; b ]) cutout cap_cutout)
      { spec = Kind.Key
      ; outer_w
      ; outer_h
      ; inner_w
      ; inner_h
      ; thickness
      ; clip
      ; cap_height
      ; clearance
      })
