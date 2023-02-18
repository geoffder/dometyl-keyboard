open OCADml
open OSCADml

type t =
  { scad : Scad.d3
  ; thickness : float [@cad.ignore]
  ; screw_l : V3.t
  ; screw_r : V3.t
  }
[@@deriving cad]

let screws t =
  let cyl =
    Scad.cylinder ~fn:32 ~height:8. 2.25
    |> Scad.ztrans (-.t.thickness -. 1.)
    |> Scad.color ~alpha:0.5 Color.Black
  in
  Scad.union [ Scad.translate t.screw_l cyl; Scad.translate t.screw_r cyl ]

let to_scad ?(show_screws = false) t =
  if show_screws then Scad.union [ t.scad; screws t ] else t.scad

let print_pcb ?(right_hand = true) thickness =
  let import n =
    let hand = if right_hand then "right" else "left" in
    Scad.import2
    @@ Printf.sprintf "%s/stls/mcu_holders/bastardkb/%s/%s.stl" Util.imports hand n
  in
  Scad.difference
    (import "shield_plate")
    [ import "shield_screwholes"; import "shield_window"; import "shield_pinholes" ]
  |> Scad.extrude ~height:thickness
  |> Scad.color ~alpha:0.5 Color.Crimson

let screw_l_start = v2 (-0.45) (-31.25)
let screw_r_start = v2 34.13 (-3.375)

let pcb thickness =
  let poly =
    Scad.polygon
    @@ Path2.of_tups
         [ 0., 0.
         ; 0.8, 1.
         ; 9.5, 1.
         ; 11., 0.
         ; 35., 0.
         ; 36.5, -0.5
         ; 37.6, -2.0
         ; 37.6, -11.5
         ; 30.8, -34.
         ; 30., -35.5
         ; 28., -35.9
         ; 0., -35.9
         ; -3., -35.
         ; -4.5, -33.
         ; -4.8, -31.
         ; -4.4, -29.
         ; -3., -27.5
         ; -1.2, -26.5
         ; 0., -24.
         ]
  in
  let hole = Scad.circle ~fn:36 2.45 in
  Scad.difference
    poly
    [ Scad.translate screw_l_start hole
    ; Scad.translate screw_r_start hole
    ; Scad.translate (v2 11.5 (-31.3)) (Scad.square (v2 17.65 15.9))
    ]
  |> Scad.extrude ~height:thickness
  |> Scad.color ~alpha:0.5 Color.Crimson

let make ?(inset_depth = 2.5) ?(thickness = 1.) ?(trrs = true) () =
  let jack_radius = 2.49
  and jack_width = 6.2
  and usb_height = 3.6
  and usb_width = 9.575
  and board_width = 21.25
  and mcu_thickness = 2.
  and dist = 15.925 in
  let jack_x_off = (jack_width /. 2.) +. 1.5 in
  let jack =
    if trrs
    then
      Scad.cylinder ~center:true ~fn:16 ~height:20. jack_radius
      |> Scad.xrot (Float.pi /. 2.)
      |> Scad.translate (v3 jack_x_off 0. 4.5)
      |> Scad.color ~alpha:0.5 Color.Silver
    else Scad.empty3
  and usb =
    let rad = usb_height /. 2. in
    let cyl =
      Scad.cylinder ~center:true ~fn:16 ~height:20. rad
      |> Scad.rotate (v3 (Float.pi /. 2.) 0. 0.)
    in
    Scad.hull
      [ Scad.translate (v3 ((usb_width /. 2.) -. rad) 0. 0.) cyl
      ; Scad.translate (v3 ((usb_width /. -2.) +. rad) 0. 0.) cyl
      ]
    |> Scad.translate (v3 (jack_x_off +. dist) 0. 4.85)
    |> Scad.color ~alpha:0.5 Color.Silver
  and inset =
    let h = jack_radius +. (usb_height /. 2.) +. mcu_thickness
    and w = ((jack_width +. board_width) /. 2.) +. dist in
    Scad.cube (v3 w 16. h)
    |> Scad.translate (v3 0. (inset_depth -. 16.) 1.)
    |> Scad.color ~alpha:0.5 Color.Silver
  in
  let t =
    { scad = pcb thickness
    ; thickness
    ; screw_l = V3.of_v2 screw_l_start
    ; screw_r = V3.of_v2 screw_r_start
    }
  in
  { t with scad = Scad.union [ t.scad; jack; usb; inset ] }

let place
    ?(x_off = 0.2)
    ?(y_off = -0.5)
    ?(z_off = 2.5)
    ?(z_rot = 0.)
    Walls.{ body = { north; _ }; _ }
    t
  =
  let left_foot = (IMap.find 0 north).foot
  and right_foot = (IMap.find 1 north).foot in
  let x = V3.x left_foot.bot_left +. x_off
  and y =
    let inner (ps : Points.t) = V3.(y ps.bot_left +. y ps.bot_right) /. 2. in
    y_off +. ((inner left_foot +. inner right_foot) /. 2.)
  in
  zrot z_rot t |> translate (v3 x y z_off)

let eyelets
    ?width
    ?(bury = 0.3)
    ?(z_off = 1.)
    ?(config = Eyelet.m4_config)
    Connect.{ inline; outline; _ }
    { screw_l; screw_r; thickness; _ }
  =
  let build p =
    Eyelet.place ?width ~bury ~config ~inline ~outline (`Loc V3.(v (x p) (y p) 0.))
    |> Eyelet.to_scad
    |> Scad.ztrans (V3.z p +. thickness +. z_off)
  in
  Scad.union [ build screw_l; build screw_r ]

let cutter
    ?eye_width
    ?eye_z_off
    ?eyelet_config
    ?x_off
    ?y_off
    ?z_off
    ?z_rot
    t
    ~walls
    ~connections
  =
  let t' = place ?x_off ?y_off ?z_off ?z_rot walls t in
  Ports.
    { plus =
        Option.some
        @@ eyelets ?width:eye_width ?z_off:eye_z_off ?config:eyelet_config connections t'
    ; minus = Option.some @@ to_scad t'
    }
