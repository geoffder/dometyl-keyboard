open! Base
open! Scad_ml

let make
    ?(length = 2.)
    ?(jack_radius = 2.65)
    ?(jack_width = 6.2)
    ?(usb_height = 3.6)
    ?(usb_width = 9.5)
    ?(board_width = 21.)
    ?(board_thickness = 2.)
    ?(dist = 15.5)
    ?(x_off = 4.)
    ?(z_off = 7.)
    Walls.{ body = { cols; _ }; _ }
  =
  let left_foot = (Option.value_exn (Map.find_exn cols 0).north).foot in
  let right_foot = (Option.value_exn (Map.find_exn cols 1).north).foot in
  let inner_y (foot : Points.t) =
    Vec3.(get_y foot.bot_left +. get_y foot.bot_right) /. 2.
  in
  let jack =
    Model.cylinder ~center:true ~fn:16 jack_radius 20.
    |> Model.rotate (Float.pi /. 2., 0., 0.)
  and usb =
    let rad = usb_height /. 2. in
    let cyl =
      Model.cylinder ~center:true ~fn:16 rad 20. |> Model.rotate (Float.pi /. 2., 0., 0.)
    in
    Model.hull
      [ Model.translate ((usb_width /. 2.) -. rad, 0., 0.) cyl
      ; Model.translate ((usb_width /. -2.) +. rad, 0., 0.) cyl
      ]
    |> Model.translate (dist, 0., 0.)
  and inset =
    let fudge = 5. (* extra y length back into case for inset *)
    and below = Float.max ((usb_height /. 2.) +. board_thickness) jack_radius in
    let h = jack_radius +. below
    and len (foot : Points.t) =
      (Vec3.(get_y foot.top_left +. get_y foot.top_right) /. 2.) -. inner_y foot
    in
    let left =
      let l = len left_foot in
      Model.cube ~center:true (jack_width, l +. fudge, h)
      |> Model.translate
           ( 0.
           , Float.max 0. (l -. length) -. (l /. 2.) -. (fudge /. 2.)
           , (jack_radius -. below) /. 2. )
    in
    let right =
      let l = len right_foot
      (* bring right into frame of reference of left inner_y *)
      and y_diff = inner_y right_foot -. inner_y left_foot in
      Model.cube ~center:true (board_width, l +. fudge, h)
      |> Model.translate
           ( dist
           , Float.max 0. (l -. length) -. (l /. 2.) +. y_diff -. (fudge /. 2.)
           , (jack_radius -. below) /. 2. )
    in
    Model.hull [ left; right ]
  in
  Model.union [ jack; usb; inset ]
  |> Model.translate Vec3.(get_x left_foot.bot_left +. x_off, inner_y left_foot, z_off)

let place_tray
    ?(x_off = 0.)
    ?(y_off = -0.25)
    ?(z_rot = 0.)
    Walls.{ body = { cols; _ }; _ }
    scad
  =
  let left_foot = (Option.value_exn (Map.find_exn cols 0).north).foot
  and right_foot = (Option.value_exn (Map.find_exn cols 1).north).foot in
  let x = Vec3.get_x left_foot.bot_left +. x_off
  and y =
    let outer (ps : Points.t) = Vec3.(get_y ps.top_left +. get_y ps.top_right) /. 2. in
    y_off +. ((outer left_foot +. outer right_foot) /. 2.)
  in
  Model.rotate (0., 0., z_rot) scad |> Model.translate (x, y, 0.)

let carbonfet_stl micro =
  let import s = Model.import (Printf.sprintf "../things/holders/carbonfet/%s.stl" s) in
  Model.color Color.FireBrick
  @@
  if micro
  then Model.translate (-108.8, -125.37, 0.) (import "pro_micro_holder")
  else Model.translate (-109.5, -123.8, 0.) (import "elite-c_holder")

let carbonfet_holder ?(micro = false) ?x_off ?y_off ?z_rot walls =
  let slab =
    Model.cube (28.266, 15., 12.)
    |> Model.translate (1.325, -8., 0.)
    |> Model.color ~alpha:0.5 Color.Salmon
  and trrs_clearance =
    Model.cube (8.4, 4., 5.)
    |> Model.translate (2.62, -6., 12.)
    |> Model.color ~alpha:0.5 Color.Salmon
  in
  place_tray
    ?x_off
    ?y_off
    ?z_rot
    walls
    (Model.union [ carbonfet_stl micro; slab; trrs_clearance ])

let derek_reversible_stl reset_button =
  (if reset_button then "elite-c_holder_w_reset" else "elite-c_holder")
  |> Printf.sprintf "../things/holders/dereknheiley/%s.stl"
  |> Model.import
  |> Model.translate (15.3, 0., 0.)
  |> Model.color Color.FireBrick

let reversible_holder ?(reset_button = false) ?x_off ?y_off ?z_rot walls =
  let h = if reset_button then 15. else 8.4 in
  let through =
    Model.cube (27.6, 12., h)
    |> Model.translate (1.5, -7., 0.)
    |> Model.color ~alpha:0.5 Color.Salmon
  and front =
    Model.cube (30.6, 4., h)
    |> Model.translate (0., -0.5, 0.)
    |> Model.color ~alpha:0.5 Color.Salmon
  in
  place_tray
    ?x_off
    ?y_off
    ?z_rot
    walls
    (Model.union [ derek_reversible_stl reset_button; through; front ])

module BastardShield = struct
  type t =
    { pcb : Model.t
    ; screw_l : Vec3.t
    ; screw_r : Vec3.t
    }

  let t =
    let offset = -7.5, -27.25, 0.5 in
    let screw_l = Vec3.((4.1, -0.15, 0.) <+> offset)
    and screw_r = Vec3.((36.15, 22.7, 0.) <+> offset)
    and pcb =
      Model.import "../things/holders/bastardkb/elite-c_shield.dxf"
      |> Model.translate offset
      |> Model.color ~alpha:0.5 Color.Silver
    in
    { pcb; screw_l; screw_r }

  let translate p t =
    { pcb = Model.translate p t.pcb
    ; screw_l = Vec3.add p t.screw_l
    ; screw_r = Vec3.add p t.screw_r
    }

  let rotate r t =
    { pcb = Model.rotate r t.pcb
    ; screw_l = Vec3.rotate r t.screw_l
    ; screw_r = Vec3.rotate r t.screw_r
    }

  let rotate_about_pt r p t =
    { pcb = Model.rotate_about_pt r p t.pcb
    ; screw_l = Vec3.rotate_about_pt r p t.screw_l
    ; screw_r = Vec3.rotate_about_pt r p t.screw_r
    }

  let screws t =
    let cyl = Model.cylinder ~center:true 2.25 5. |> Model.color ~alpha:0.5 Color.Black in
    Model.union [ Model.translate t.screw_l cyl; Model.translate t.screw_r cyl ]

  let to_scad ?(show_screws = false) t =
    if show_screws then Model.union [ t.pcb; screws t ] else t.pcb
end
