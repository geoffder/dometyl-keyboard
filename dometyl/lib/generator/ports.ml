open! Base
open! Scad_ml

type t =
  { plus : Scad.d3 option
  ; minus : Scad.d3 option
  }

type cutter = walls:Walls.t -> connections:Connect.t -> t

let apply t scad =
  let added =
    Option.value_map ~default:scad ~f:(fun s -> Scad.union [ s; scad ]) t.plus
  in
  Option.value_map ~default:added ~f:(fun s -> Scad.difference added [ s ]) t.minus

let make
    ?(length = 2.)
    ?(jack_radius = 2.49)
    ?(jack_width = 6.2)
    ?(usb_height = 3.6)
    ?(usb_width = 9.575)
    ?(usb_z_off = 0.35)
    ?(board_width = 21.25)
    ?(board_thickness = 2.)
    ?(dist = 15.925)
    ?(x_off = 3.3)
    ?(z_off = 6.4)
    ()
    ~walls:Walls.{ body = { north; _ }; _ }
    ~connections:_
  =
  let left_foot = (Map.find_exn north 0).foot
  and right_foot = (Map.find_exn north 1).foot in
  let inner_y (foot : Points.t) =
    V3.(get_y foot.bot_left +. get_y foot.bot_right) /. 2.
  in
  let jack =
    Scad.cylinder ~center:true ~fn:16 ~height:20. jack_radius |> Scad.xrot (Float.pi /. 2.)
  and usb =
    let rad = usb_height /. 2. in
    let cyl =
      Scad.cylinder ~center:true ~fn:16 ~height:20. rad |> Scad.xrot (Float.pi /. 2.)
    in
    Scad.hull
      [ Scad.translate (v3 ((usb_width /. 2.) -. rad) 0. 0.) cyl
      ; Scad.translate (v3 ((usb_width /. -2.) +. rad) 0. 0.) cyl
      ]
    |> Scad.translate (v3 dist 0. usb_z_off)
  and inset =
    let fudge = 5. (* extra y length back into case for inset *)
    and below = Float.max ((usb_height /. 2.) +. board_thickness) jack_radius in
    let h = jack_radius +. below
    and len (foot : Points.t) =
      (V3.(get_y foot.top_left +. get_y foot.top_right) /. 2.) -. inner_y foot
    in
    let left =
      let l = len left_foot in
      let y = Float.max 0. (l -. length) -. (l /. 2.) -. (fudge /. 2.) in
      Scad.cube ~center:true (v3 jack_width (l +. fudge) h)
      |> Scad.translate (v3 0. y ((jack_radius -. below) /. 2.))
    in
    let right =
      let l = len right_foot
      (* bring right into frame of reference of left inner_y *)
      and y_diff = inner_y right_foot -. inner_y left_foot in
      Scad.cube ~center:true (v3 board_width (l +. fudge) h)
      |> Scad.translate
           (v3
              dist
              (Float.max 0. (l -. length) -. (l /. 2.) +. y_diff -. (fudge /. 2.))
              ((jack_radius -. below) /. 2.) )
    in
    Scad.hull [ left; right ]
  in
  let cutout =
    Scad.union [ jack; usb; inset ]
    |> Scad.translate
         V3.(v3 (get_x left_foot.bot_left +. x_off) (inner_y left_foot) z_off)
  in
  { plus = None; minus = Some cutout }

let place_tray
    ?(x_off = 0.)
    ?(y_off = -0.25)
    ?(z_rot = 0.)
    Walls.{ body = { north; _ }; _ }
    scad
  =
  let left = Map.find north 0
  and right = Map.find north 1 in
  let x =
    match Option.first_some left right with
    | Some w -> V3.get_x w.foot.bot_left +. x_off
    | None -> 0.
  and y =
    let outer =
      let f (ps : Points.t) = V3.(get_y ps.top_left +. get_y ps.top_right) /. 2. in
      Option.value_map ~default:0. ~f:(fun w -> f w.Wall.foot)
    in
    y_off +. ((outer left +. outer right) /. 2.)
  in
  Scad.zrot z_rot scad |> Scad.translate (v3 x y 0.)

let carbonfet_stl micro =
  let import s = Scad.import3 (Printf.sprintf "../things/holders/carbonfet/%s.stl" s) in
  Scad.color Color.FireBrick
  @@
  if micro
  then Scad.translate (v3 (-108.8) (-125.37) 0.) (import "pro_micro_holder")
  else Scad.translate (v3 (-109.5) (-123.8) 0.) (import "elite-c_holder")

let carbonfet_holder ?(micro = false) ?x_off ?y_off ?z_rot () ~walls ~connections:_ =
  let slab =
    Scad.cube (v3 28.266 15. 12.)
    |> Scad.translate (v3 1.325 (-8.) 0.)
    |> Scad.color ~alpha:0.5 Color.Salmon
  and trrs_clearance =
    Scad.cube (v3 8.4 4. 5.)
    |> Scad.translate (v3 2.62 (-6.) 12.)
    |> Scad.color ~alpha:0.5 Color.Salmon
  in
  let tray =
    place_tray
      ?x_off
      ?y_off
      ?z_rot
      walls
      (Scad.union [ carbonfet_stl micro; slab; trrs_clearance ])
  in
  { plus = None; minus = Some tray }

let derek_reversible_stl reset_button =
  (if reset_button then "elite-c_holder_w_reset" else "elite-c_holder")
  |> Printf.sprintf "../things/holders/dereknheiley/%s.stl"
  |> Scad.import3
  |> Scad.translate (v3 15.3 0. 0.)
  |> Scad.color Color.FireBrick

let reversible_holder
    ?(reset_button = false)
    ?(rail_w = 1.4)
    ?x_off
    ?y_off
    ?z_rot
    ()
    ~walls
    ~connections:_
  =
  let w = 30.6
  and h = if reset_button then 15. else 8.4 in
  let tray =
    Scad.cube (v3 27.6 38.8 7.7)
    |> Scad.translate (v3 3. (-38.8) 0.)
    |> Scad.color ~alpha:0.5 Color.Salmon
  and front =
    Scad.cube (v3 w 13. h)
    |> Scad.translate (v3 0. (-8.) 0.)
    |> Scad.color ~alpha:0.5 Color.Salmon
  and rails =
    let rail = Scad.cube ~center:true (v3 (rail_w +. 0.01) rail_w (h +. 0.01)) in
    [ Scad.translate (v3 (rail_w /. 2.) (-2.25) (h /. 2.)) rail
    ; Scad.translate (v3 (w -. (rail_w /. 2.)) (-2.25) (h /. 2.)) rail
    ]
  in
  let minus =
    Scad.difference (Scad.union [ tray; front ]) rails
    |> place_tray ?x_off ?y_off ?z_rot walls
    |> Option.some
  in
  { plus = None; minus }
