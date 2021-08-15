open! Base
open! Scad_ml

(* NOTE: This is still rough, as it is using a rectangle as inset and it along
   with the ports are oriented exactly along the y-axis. This is despite the fact
   the inner index column (leftmost) can be angled, and that it is independent from
   the main index column. Thus, the walls that the jack and usb are distributed
   across can be "kinked", leading to uneven penetration of the inset into the walls,
   and thus resulting in uneven outer wall thickness and port length. *)
let make
    ?(length = 2.)
    ?(jack_radius = 2.65)
    ?(jack_width = 6.2)
    ?(usb_height = 3.6)
    ?(usb_width = 9.5)
    ?(board_width = 20.)
    ?(board_thickness = 2.)
    ?(dist = 15.5)
    ?(x_off = 6.)
    ?(z_off = 7.)
    Walls.{ body = { cols; _ }; _ }
  =
  let foot = (Option.value_exn (Map.find_exn cols 0).north).foot in
  let inner_y = Vec3.(get_y foot.bot_left +. get_y foot.bot_right) /. 2. in
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
    let w = ((jack_width +. board_width) /. 2.) +. dist
    and l = (Vec3.(get_y foot.top_left +. get_y foot.top_right) /. 2.) -. inner_y
    and below = Float.max ((usb_height /. 2.) +. board_thickness) jack_radius in
    Model.cube ~center:true (w, l, jack_radius +. below)
    |> Model.translate
         ( (w -. jack_width) /. 2.
         , Float.max 0. (l -. length) -. (l /. 2.)
         , (jack_radius -. below) /. 2. )
  in
  Model.union [ jack; usb; inset ]
  |> Model.translate Vec3.(get_x foot.bot_left +. x_off, inner_y, z_off)
