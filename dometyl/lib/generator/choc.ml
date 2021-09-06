open! Base
open! Scad_ml

(* https://grabcad.com/library/kailh-1350-socket-2 *)
let kailh_socket =
  Model.import "../things/choc_hotswap_socket.stl"
  |> Model.translate (7., 0., 0.)
  |> Model.rotate (Float.pi /. 2., 0., Float.pi)
  |> Model.translate (2.0, 3.7, -3.5)
  |> Model.color ~alpha:0.4 Color.Silver

(* https://grabcad.com/library/kailh-low-profile-mechanical-keyboard-switch-1 *)
let switch =
  Model.import "../things/kailh_choc.stl"
  |> Model.rotate Float.(pi / 2., 0., pi / 2.)
  |> Model.translate (0., 0., 0.4)
  |> Model.color ~alpha:0.5 Color.SkyBlue

module Hotswap = struct
  let make ~inner_w ~inner_h ~outer_w ~outer_h ~plate_thickness facing =
    let holder_thickness = 3.
    and hole_depth = 2.2 in
    let shallowness = hole_depth -. plate_thickness in
    (* hotswap socket cutout position *)
    let w = inner_w +. 3.
    and h = inner_h +. 3.
    and z = (holder_thickness +. hole_depth +. shallowness) /. -2.
    (* the bottom of the hole.  *)
    and socket_thickness = holder_thickness +. 0.5 (* plus printing error *)
    and pin_radius = 1.5
    and sign =
      match facing with
      | `North -> -1.
      | `South -> 1.
    in
    let socket_z = z -. 1.4 in
    let cutout =
      let edge_x = 0.2 +. (outer_w /. 2.)
      and edge_y = 0.2 +. (outer_h /. 2.) in
      let poly =
        Model.polygon
        @@ List.map
             ~f:(fun (x, y) -> x *. sign, y *. sign)
             [ -.edge_x, edge_y
             ; -5., 3.45
             ; 0.75, 3.45
             ; 1.3, 3.2
             ; 1.6, 3.
             ; 1.88, 2.7
             ; 2.4, 1.7
             ; 2.8, 1.35
             ; edge_x, 1.35
             ; edge_x, edge_y
             ; 7.2, edge_y
             ; 7.2, 6.05
             ; 4., 6.05
             ; 3.4, 6.1
             ; 3.1, 6.2
             ; 2.9, 6.3
             ; 2.6, 6.7
             ; 2.5, 7.0
             ; 2.5, 7.4
             ; 1.2, edge_y
             ; -5., edge_y
             ]
      and pin =
        Model.translate (0., 5.9 *. sign, 0.) (Model.cylinder ~fn:30 pin_radius 2.4)
      in
      Model.union
        [ poly |> Model.linear_extrude ~center:true ~height:socket_thickness; pin ]
      |> Model.translate (0., 0., socket_z)
    in
    let hotswap =
      let led_cut =
        Model.square ~center:true (6., 6.) |> Model.translate (0., -6. *. sign, 0.)
      and holes =
        let main = Model.circle ~fn:30 1.7
        and pin = Model.circle ~fn:30 pin_radius
        and friction = Model.circle ~fn:30 0.975 in
        let plus = Model.translate (0., 5.9 *. sign, 0.) pin
        and minus = Model.translate (5. *. sign, 3.8 *. sign, 0.) pin
        and fric_left = Model.translate (-5.5, 0., 0.) friction
        and fric_right = Model.translate (5.5, 0., 0.) friction in
        Model.union [ main; plus; minus; fric_left; fric_right ]
      in
      Model.difference (Model.square ~center:true (w, h)) [ led_cut; holes ]
      |> Model.linear_extrude ~center:true ~height:holder_thickness
      |> Model.translate (0., 0., z)
      |> Fn.flip Model.difference [ cutout ]
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
end

let teeth ~inner_w ~thickness hole =
  let depth = 1.2 in
  let block = Model.cube ~center:true (0.51, 3., thickness -. depth)
  and x = (inner_w /. 2.) +. 0.25
  and y = 3.5 in
  let nw = Model.translate (-.x, y, -.depth) block
  and sw = Model.translate (-.x, -.y, -.depth) block
  and ne = Model.translate (x, y, -.depth) block
  and se = Model.translate (x, -.y, -.depth) block in
  Model.difference hole [ nw; sw; ne; se ]

let make_hole
    ?cap
    ?hotswap
    ?(outer_w = 19.)
    ?(outer_h = 17.)
    ?(inner_w = 13.8)
    ?(inner_h = 13.8)
    ?(thickness = 4.)
    ?(cap_height = 4.4)
    ?(clearance = 3.)
    ()
  =
  let clearance, clip, cutout =
    match hotswap with
    | Some facing ->
      let swap, cutout =
        Hotswap.make ~inner_w ~inner_h ~outer_w ~outer_h ~plate_thickness:thickness facing
      in
      let clip hole = Model.union [ teeth ~inner_w ~thickness hole; swap ] in
      clearance +. 3., clip, Some cutout
    | None        -> clearance, teeth ~inner_w ~thickness, None
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

let example_assembly
    ?(show_cutout = false)
    ?(show_switch = false)
    ?(show_socket = false)
    ?(show_cap = false)
    ()
  =
  let hole = make_hole ~hotswap:`South ~cap:Caps.mbk () in
  let cutout = Option.value_exn hole.cutout in
  let hole =
    KeyHole.cutout_scad hole
    |> Model.translate (0., 0., -2.)
    |> Model.color ~alpha:0.5 Color.FireBrick
  and cutout =
    if show_cutout
    then
      Some (Model.translate (0., 0., -2.) cutout |> Model.color Color.DarkGray ~alpha:0.5)
    else None
  and choc = Option.some_if show_switch switch
  and socket = Option.some_if show_socket kailh_socket
  and cap = Option.bind ~f:(Option.some_if show_cap) hole.cap in
  Util.prepend_opt cutout [ hole ]
  |> Util.prepend_opt choc
  |> Util.prepend_opt socket
  |> Util.prepend_opt cap
  |> Model.union
