open! Base
open! Scad_ml

module ThroughHole = struct
  type config =
    { leg_w : float
    ; leg_thickness : float
    ; leg_l : float
    ; leg_spacing : float
    ; leg_bend : float
    ; leg_z_offset : float (* from body centre *)
    ; merge_legs : bool
    ; body_w : float
    ; body_l : float
    ; body_thickness : float
    }

  let default =
    { leg_w = 0.5
    ; leg_thickness = 0.4
    ; leg_l = 20.
    ; leg_spacing = 1.
    ; leg_bend = 4.
    ; leg_z_offset = -0.2
    ; merge_legs = false
    ; body_w = 4.
    ; body_l = 3.
    ; body_thickness = 1.5
    }

  let default_print =
    { leg_w = 0.6
    ; leg_thickness = 0.85
    ; leg_l = 20.
    ; leg_spacing = 1.25
    ; leg_bend = 4.
    ; leg_z_offset = -0.2
    ; merge_legs = true
    ; body_w = 4.2
    ; body_l = 3.2
    ; body_thickness = 1.5
    }

  type t =
    { config : config
    ; scad : Scad.d3
    }

  let make
      ?(leg_w = 0.5)
      ?(leg_thickness = 0.4)
      ?(leg_l = 20.)
      ?(leg_spacing = 1.)
      ?(leg_bend = 4.)
      ?(leg_z_offset = -0.2)
      ?(merge_legs = false)
      ?(body_w = 4.)
      ?(body_l = 3.)
      ?(body_thickness = 1.5)
      () =
    let bent_leg =
      let start =
        Scad.cube ~center:true (leg_w, leg_bend, leg_thickness)
        |> Scad.translate (0., (leg_bend +. body_l) /. 2., 0.)
      and rest =
        Scad.cube ~center:true (leg_w, leg_thickness, leg_l -. leg_bend)
        |> Scad.translate
             ( 0.
             , leg_bend +. ((body_l -. leg_w) /. 2.)
             , (leg_l -. leg_bend -. leg_thickness) /. -2. )
      in
      Scad.union [ start; rest ] |> Scad.translate (0., 0., leg_z_offset)
    in
    let legs =
      let side_offset = leg_spacing +. (leg_w /. 2.) in
      if not merge_legs then
        Scad.union
          [ bent_leg
          ; Scad.translate (-.side_offset, 0., 0.) bent_leg
          ; Scad.translate (side_offset, 0., 0.) bent_leg
          ]
      else Scad.scale ((2. *. side_offset /. leg_w) +. 1., 1., 1.) bent_leg
    and body = Scad.cube ~center:true (body_w, body_l +. 0.001, body_thickness) in
    Scad.union [ body; legs ] |> Scad.translate (0., -0.0005, body_thickness /. 2.)

  (* TODO: this is the replacement for the sink workflow. This is more generic
     and easier to understand. I'll keep code that can generate sensor models
     for visualization, but this cutout can be used for bulk or tape through hole
     sensors. can make bulk and tape configs / uses of make so there are
     existing rough examples available. SOD sensors would of course need their
     own very different cutout function. *)
  let cutout
      ?(body_w = 4.2)
      ?(body_l = 3.2)
      ?(legs_w = 3.8)
      ?(legs_l = 4.)
      ?(legs_z_offset = 0.4)
      ?(slot_l = 0.85)
      ?(z = 0.)
      depth =
    let body = Scad.cube ~center:true (body_w, body_l +. 0.001, depth)
    and legs =
      Scad.cube ~center:true (legs_w, legs_l, depth -. legs_z_offset)
      |> Scad.translate (0., (legs_l +. body_l) /. 2., legs_z_offset /. 2.)
    and slot =
      Scad.cube ~center:true (legs_w, slot_l, depth +. 4.)
      |> Scad.translate (0., legs_l +. ((body_l -. slot_l) /. 2.), -2.)
    in
    Scad.union [ body; legs; slot ] |> Scad.translate (0., 0., z +. (depth /. -2.))

  let of_config
      ( { leg_w
        ; leg_thickness
        ; leg_l
        ; leg_spacing
        ; leg_bend
        ; leg_z_offset
        ; merge_legs
        ; body_w
        ; body_l
        ; body_thickness
        } as config ) =
    let bent_leg =
      let start =
        Scad.cube ~center:true (leg_w, leg_bend, leg_thickness)
        |> Scad.translate (0., (leg_bend +. body_l) /. 2., 0.)
      and rest =
        Scad.cube ~center:true (leg_w, leg_thickness, leg_l -. leg_bend)
        |> Scad.translate
             ( 0.
             , leg_bend +. ((body_l -. leg_w) /. 2.)
             , (leg_l -. leg_bend -. leg_thickness) /. -2. )
      in
      Scad.union [ start; rest ] |> Scad.translate (0., 0., leg_z_offset)
    in
    let legs =
      let side_offset = leg_spacing +. (leg_w /. 2.) in
      if not merge_legs then
        Scad.union
          [ bent_leg
          ; Scad.translate (-.side_offset, 0., 0.) bent_leg
          ; Scad.translate (side_offset, 0., 0.) bent_leg
          ]
      else Scad.scale ((2. *. side_offset /. leg_w) +. 1., 1., 1.) bent_leg
    and body = Scad.cube ~center:true (body_w, body_l +. 0.001, body_thickness) in
    { config
    ; scad =
        Scad.union [ body; legs ] |> Scad.translate (0., -0.0005, body_thickness /. 2.)
    }

  let sink ?z { config; scad } depth =
    let scad =
      Option.value_map ~default:scad ~f:(fun z -> Scad.translate (0., 0., z) scad) z
    in
    let f i =
      Scad.translate (0., 0., Float.of_int i *. config.leg_thickness /. -2.) scad
    in
    Scad.union @@ List.init (Int.of_float (depth /. config.leg_w *. 2.) + 1) ~f
end
