open! Base
open! Scad_ml

module Config = struct
  type t =
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

  let a3144 =
    { leg_w = 0.5
    ; leg_thickness = 0.4
    ; leg_l = 20.
    ; leg_spacing = 1.
    ; leg_bend = 4.
    ; leg_z_offset = -0.2
    ; merge_legs = false
    ; body_w = 3.93
    ; body_l = 3.
    ; body_thickness = 1.5
    }

  let a3144_print =
    { leg_w = 0.6
    ; leg_thickness = 0.85
    ; leg_l = 20.
    ; leg_spacing = 1.25
    ; leg_bend = 4.
    ; leg_z_offset = -0.2
    ; merge_legs = true
    ; body_w = 4.4
    ; body_l = 3.4
    ; body_thickness = 1.5
    }
end

type t =
  { config : Config.t
  ; scad : Model.t
  }

let make
    ( Config.
        { leg_w
        ; leg_thickness
        ; leg_l
        ; leg_spacing
        ; leg_bend
        ; leg_z_offset
        ; merge_legs
        ; body_w
        ; body_l
        ; body_thickness
        } as config )
  =
  let bent_leg =
    let start =
      Model.cube ~center:true (leg_w, leg_bend, leg_thickness)
      |> Model.translate (0., (leg_bend +. body_l) /. 2., 0.)
    and rest =
      Model.cube ~center:true (leg_w, leg_thickness, leg_l -. leg_bend)
      |> Model.translate
           ( 0.
           , leg_bend +. ((body_l -. leg_w) /. 2.)
           , (leg_l -. leg_bend -. leg_thickness) /. -2. )
    in
    Model.union [ start; rest ] |> Model.translate (0., 0., leg_z_offset)
  in
  let legs =
    let side_offset = leg_spacing +. (leg_w /. 2.) in
    if not merge_legs
    then
      Model.union
        [ bent_leg
        ; Model.translate (-.side_offset, 0., 0.) bent_leg
        ; Model.translate (side_offset, 0., 0.) bent_leg
        ]
    else Model.scale ((2. *. side_offset /. leg_w) +. 1., 1., 1.) bent_leg
  and body = Model.cube ~center:true (body_w, body_l, body_thickness) in
  { config
  ; scad = Model.union [ body; legs ] |> Model.translate (0., 0., body_thickness /. 2.)
  }

let sink { config; scad } depth =
  let f i =
    Model.translate (0., 0., Float.of_int i *. config.leg_thickness /. -2.) scad
  in
  Model.union @@ List.init (Int.of_float (depth /. config.leg_w *. 2.) + 1) ~f
