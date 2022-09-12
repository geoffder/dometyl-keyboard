open! Scad_ml

type cutter = ?z:float -> float -> Scad.d3

module ThroughHole = struct
  let bulk_model
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
      ()
    =
    let bent_leg =
      let start =
        Scad.cube ~center:true (v3 leg_w leg_bend leg_thickness)
        |> Scad.translate (v3 0. ((leg_bend +. body_l) /. 2.) 0.)
      and rest =
        Scad.cube ~center:true (v3 leg_w leg_thickness (leg_l -. leg_bend))
        |> Scad.translate
             (v3
                0.
                (leg_bend +. ((body_l -. leg_w) /. 2.))
                ((leg_l -. leg_bend -. leg_thickness) /. -2.) )
      in
      Scad.union [ start; rest ] |> Scad.translate (v3 0. 0. leg_z_offset)
    in
    let legs =
      let side_offset = leg_spacing +. (leg_w /. 2.) in
      if not merge_legs
      then
        Scad.union
          [ bent_leg
          ; Scad.translate (v3 (-.side_offset) 0. 0.) bent_leg
          ; Scad.translate (v3 side_offset 0. 0.) bent_leg
          ]
      else Scad.scale (v3 ((2. *. side_offset /. leg_w) +. 1.) 1. 1.) bent_leg
    and body = Scad.cube ~center:true (v3 body_w (body_l +. 0.001) body_thickness) in
    Scad.union [ body; legs ] |> Scad.translate (v3 0. (-0.0005) (body_thickness /. 2.))

  let bulk_cutout
      ?(body_w = 4.2)
      ?(body_l = 3.2)
      ?(legs_w = 3.8)
      ?(legs_l = 4.)
      ?(legs_z_offset = 0.4)
      ?(slot_l = 0.85)
      ()
      ?(z = 0.)
      depth
    =
    let body = Scad.cube ~center:true (v3 body_w (body_l +. 0.001) depth)
    and legs =
      Scad.cube ~center:true (v3 legs_w legs_l (depth -. legs_z_offset))
      |> Scad.translate (v3 0. ((legs_l +. body_l) /. 2.) (legs_z_offset /. 2.))
    and slot =
      Scad.cube ~center:true (v3 legs_w slot_l (depth +. 4.))
      |> Scad.translate (v3 0. (legs_l +. ((body_l -. slot_l) /. 2.)) (-2.))
    in
    Scad.union [ body; legs; slot ] |> Scad.translate (v3 0. 0. (z +. (depth /. -2.)))

  let tape_cutout
      ?(body_w = 4.2)
      ?(body_l = 3.2)
      ?(legs_w1 = 3.8)
      ?(legs_w2 = 6.)
      ?(legs_l1 = 1.2)
      ?(legs_l2 = 2.8)
      ?(legs_z_offset = 0.4)
      ?(slot_l = 0.85)
      ()
      ?(z = 0.)
      depth
    =
    let body = Scad.cube ~center:true (v3 body_w (body_l +. 0.001) depth)
    and legs =
      let thickness = depth -. legs_z_offset in
      Scad.union
        [ Scad.cube ~center:true (v3 legs_w1 legs_l1 thickness)
        ; Scad.cube ~center:true (v3 legs_w2 legs_l2 thickness)
          |> Scad.translate (v3 0. (legs_l2 /. 2.) 0.)
        ]
      |> Scad.translate (v3 0. ((legs_l1 +. body_l) /. 2.) (legs_z_offset /. 2.))
    and slot =
      Scad.cube ~center:true (v3 legs_w2 slot_l (depth +. 4.))
      |> Scad.translate (v3 0. (legs_l1 +. legs_l2 +. ((body_l -. slot_l) /. 2.)) (-2.))
    in
    Scad.union [ body; legs; slot ] |> Scad.translate (v3 0. 0. (z +. (depth /. -2.)))
end
