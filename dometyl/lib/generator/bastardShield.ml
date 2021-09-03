open! Base
open! Scad_ml

type t =
  { scad : Model.t
  ; thickness : float
  ; screw_l : Vec3.t
  ; screw_r : Vec3.t
  }

let translate p t =
  { t with
    scad = Model.translate p t.scad
  ; screw_l = Vec3.add p t.screw_l
  ; screw_r = Vec3.add p t.screw_r
  }

let rotate r t =
  { t with
    scad = Model.rotate r t.scad
  ; screw_l = Vec3.rotate r t.screw_l
  ; screw_r = Vec3.rotate r t.screw_r
  }

let rotate_about_pt r p t =
  { t with
    scad = Model.rotate_about_pt r p t.scad
  ; screw_l = Vec3.rotate_about_pt r p t.screw_l
  ; screw_r = Vec3.rotate_about_pt r p t.screw_r
  }

let screws t =
  let cyl =
    Model.cylinder ~fn:32 2.25 8.
    |> Model.translate (0., 0., -.t.thickness -. 1.)
    |> Model.color ~alpha:0.5 Color.Black
  in
  Model.union [ Model.translate t.screw_l cyl; Model.translate t.screw_r cyl ]

let to_scad ?(show_screws = false) t =
  if show_screws then Model.union [ t.scad; screws t ] else t.scad

let pcb thickness =
  let import n = Model.import (Printf.sprintf "../things/holders/bastardkb/%s.svg" n) in
  Model.difference
    (import "shield_plate")
    [ import "shield_screwholes"; import "shield_window" ]
  |> Model.linear_extrude ~height:thickness
  |> Model.color ~alpha:0.5 Color.Crimson

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
    Model.cylinder ~center:true ~fn:16 jack_radius 20.
    |> Model.rotate (Float.pi /. 2., 0., 0.)
    |> Model.translate (jack_x_off, 0., 4.5)
    |> Model.color ~alpha:0.5 Color.Silver
  and usb =
    let rad = usb_height /. 2. in
    let cyl =
      Model.cylinder ~center:true ~fn:16 rad 20. |> Model.rotate (Float.pi /. 2., 0., 0.)
    in
    Model.hull
      [ Model.translate ((usb_width /. 2.) -. rad, 0., 0.) cyl
      ; Model.translate ((usb_width /. -2.) +. rad, 0., 0.) cyl
      ]
    |> Model.translate (jack_x_off +. dist, 0., 4.85)
    |> Model.color ~alpha:0.5 Color.Silver
  and inset =
    let h = jack_radius +. (usb_height /. 2.) +. mcu_thickness
    and w = ((jack_width +. board_width) /. 2.) +. dist in
    Model.cube (w, 16., h)
    |> Model.translate (0., inset_depth -. 16., 1.)
    |> Model.color ~alpha:0.5 Color.Silver
  in
  let t =
    { scad = pcb thickness
    ; thickness
    ; screw_l = 4.2, 4.6, 0.
    ; screw_r = 38.78, 32.475, 0.
    }
    |> translate (-4.65, -35.85, 0.)
  in
  { t with scad = Model.union [ t.scad; jack; usb; inset ] }

let place
    ?(x_off = 0.2)
    ?(y_off = -0.25)
    ?(z_off = 1.9)
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
    ?(screw_config = Screw.m4_config)
    Connect.{ inline; _ }
    { screw_l; screw_r; thickness; _ }
  =
  let perim = Array.of_list inline
  and half_width =
    Option.value_map ~default:(screw_config.outer_rad +. 3.) ~f:(( *. ) 0.5) width
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
    Model.union
      [ Screw.(
          make ~placement:(Point loc) screw_config (side_point false) (side_point true)
          |> to_scad)
        |> Model.translate (0., 0., Vec3.get_z screw_l +. thickness +. z_off)
      ]
  in
  Model.union [ build screw_l; build screw_r ]

let cutter
    ?eye_width
    ?eye_z_off
    ?screw_config
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
        @@ eyelets ?width:eye_width ?z_off:eye_z_off ?screw_config connections t'
    ; minus = Option.some @@ to_scad t'
    }
