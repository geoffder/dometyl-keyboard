open! Base
open! Scad_ml

type t =
  { scad : Scad.t
  ; thickness : float [@scad.ignore]
  ; screw_l : Vec3.t
  ; screw_r : Vec3.t
  }
[@@deriving scad]

let screws t =
  let cyl =
    Scad.cylinder ~fn:32 2.25 8.
    |> Scad.translate (0., 0., -.t.thickness -. 1.)
    |> Scad.color ~alpha:0.5 Color.Black
  in
  Scad.union [ Scad.translate t.screw_l cyl; Scad.translate t.screw_r cyl ]

let to_scad ?(show_screws = false) t =
  if show_screws then Scad.union [ t.scad; screws t ] else t.scad

let print_pcb thickness =
  let import n = Scad.import (Printf.sprintf "../things/holders/bastardkb/%s.svg" n) in
  Scad.difference
    (import "shield_plate")
    [ import "shield_screwholes"; import "shield_window"; import "shield_pinholes" ]
  |> Scad.linear_extrude ~height:thickness
  |> Scad.color ~alpha:0.5 Color.Crimson

let screw_l_start = -0.45, -31.25, 0.
let screw_r_start = 34.13, -3.375, 0.

let pcb thickness =
  let poly =
    Scad.polygon
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
    ; Scad.translate (11.5, -31.3, 0.) (Scad.square (17.65, 15.9))
    ]
  |> Scad.linear_extrude ~height:thickness
  |> Scad.color ~alpha:0.5 Color.Crimson

let make ?(inset_depth = 2.5) ?(thickness = 1.) () =
  let jack_radius = 2.49
  and jack_width = 6.2
  and usb_height = 3.6
  and usb_width = 9.575
  and board_width = 21.25
  and mcu_thickness = 2.
  and dist = 15.925 in
  let jack_x_off = (jack_width /. 2.) +. 1.5 in
  let jack =
    Scad.cylinder ~center:true ~fn:16 jack_radius 20.
    |> Scad.rotate (Float.pi /. 2., 0., 0.)
    |> Scad.translate (jack_x_off, 0., 4.5)
    |> Scad.color ~alpha:0.5 Color.Silver
  and usb =
    let rad = usb_height /. 2. in
    let cyl =
      Scad.cylinder ~center:true ~fn:16 rad 20. |> Scad.rotate (Float.pi /. 2., 0., 0.)
    in
    Scad.hull
      [ Scad.translate ((usb_width /. 2.) -. rad, 0., 0.) cyl
      ; Scad.translate ((usb_width /. -2.) +. rad, 0., 0.) cyl
      ]
    |> Scad.translate (jack_x_off +. dist, 0., 4.85)
    |> Scad.color ~alpha:0.5 Color.Silver
  and inset =
    let h = jack_radius +. (usb_height /. 2.) +. mcu_thickness
    and w = ((jack_width +. board_width) /. 2.) +. dist in
    Scad.cube (w, 16., h)
    |> Scad.translate (0., inset_depth -. 16., 1.)
    |> Scad.color ~alpha:0.5 Color.Silver
  in
  let t =
    { scad = pcb thickness; thickness; screw_l = screw_l_start; screw_r = screw_r_start }
  in
  { t with scad = Scad.union [ t.scad; jack; usb; inset ] }

let place
    ?(x_off = 0.2)
    ?(y_off = -0.5)
    ?(z_off = 2.5)
    ?(z_rot = 0.)
    Walls.{ body = { cols; _ }; _ }
    t
  =
  let left_foot = (Option.value_exn (Map.find_exn cols 0).north).foot
  and right_foot = (Option.value_exn (Map.find_exn cols 1).north).foot in
  let x = Vec3.get_x left_foot.bot_left +. x_off
  and y =
    let inner (ps : Points.t) = Vec3.(get_y ps.bot_left +. get_y ps.bot_right) /. 2. in
    y_off +. ((inner left_foot +. inner right_foot) /. 2.)
  in
  rotate (0., 0., z_rot) t |> translate (x, y, z_off)

let eyelets
    ?width
    ?(z_off = 1.)
    ?(eyelet_config = Eyelet.m4_config)
    Connect.{ inline; _ }
    { screw_l; screw_r; thickness; _ }
  =
  let perim = Array.of_list inline
  and half_width =
    Option.value_map ~default:(eyelet_config.outer_rad +. 3.) ~f:(( *. ) 0.5) width
  in
  let n_pts = Array.length perim in
  let dist a b = Vec3.(norm (a <-> b)) in
  let find a =
    let f i (m, idx, closest) b =
      let m' = dist a b in
      if Float.(m' < m) then m', i, b else m, idx, closest
    in
    let _, idx, closest = Array.foldi ~init:(Float.max_value, 0, Vec3.zero) ~f perim in
    idx, closest
  in
  let wrap i = if i < 0 then n_pts + i else if i >= n_pts then n_pts - i else i in
  let build loc =
    let idx, closest = find loc in
    let cw, neighbour =
      let budge p = Vec3.(add (map (( *. ) 0.1) (sub p closest)) closest)
      and ccw_idx = wrap (idx - 1)
      and cw_idx = wrap (idx + 1) in
      let ccw = budge perim.(ccw_idx)
      and cw = budge perim.(cw_idx) in
      if Float.(dist loc cw < dist loc ccw) then true, cw else false, ccw
    in
    let centre =
      let diff = Vec3.sub neighbour closest
      and step = 0.05 in
      let rec loop last_dist last frac =
        let next = Vec3.(add (map (( *. ) frac) diff) closest) in
        let next_dist = dist loc next in
        if Float.(next_dist > last_dist) then last else loop next_dist next (frac +. step)
      in
      loop (dist loc closest) closest step
    in
    let side_point neighbour_wise =
      let step i = wrap @@ if Bool.equal neighbour_wise cw then i + 1 else i - 1 in
      let rec loop acc_dist last_pos i =
        let pos = perim.(i) in
        let acc_dist' = dist pos last_pos +. acc_dist in
        if Float.(acc_dist' < half_width)
        then loop acc_dist' pos (step i)
        else (
          let vec = Vec3.(normalize (pos <-> last_pos)) in
          Vec3.(add (map (( *. ) (half_width -. acc_dist)) vec) last_pos) )
      in
      (* Check that we aren't too far from the original closest point before
           stepping over it. *)
      let drift = Vec3.sub perim.(idx) centre in
      if (not neighbour_wise) && Float.(Vec3.norm drift > half_width)
      then Vec3.(add centre (mul_scalar (Vec3.normalize drift) half_width))
      else loop 0. centre (step idx)
    in
    Scad.union
      [ Eyelet.(
          make ~placement:(Point loc) eyelet_config (side_point false) (side_point true)
          |> to_scad)
        |> Scad.translate (0., 0., Vec3.get_z screw_l +. thickness +. z_off)
      ]
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
        @@ eyelets ?width:eye_width ?z_off:eye_z_off ?eyelet_config connections t'
    ; minus = Option.some @@ to_scad t'
    }
