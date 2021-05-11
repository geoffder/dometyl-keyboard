open! Base
open! Scad_ml

module type Config = sig
  val leg_w : float
  val leg_thickness : float
  val leg_l : float
  val leg_spacing : float
  val leg_bend : float
  val leg_z_offset : float (* from body centre *)

  val merge_legs : bool
  val body_w : float
  val body_l : float
  val body_thickness : float
end

module type S = sig
  include Config

  val scad : Model.t

  (* List of intermediary scads translated downward in z, for
   * use with Model.difference. *)
  val sink : float -> Model.t list
end

module A3144Config : Config = struct
  (* Based on measurements of the sensor itself, not fudged for tolerances. *)
  let leg_w = 0.5
  let leg_thickness = 0.4
  let leg_l = 20.
  let leg_spacing = 1.
  let leg_bend = 4.
  let leg_z_offset = -0.2
  let merge_legs = false
  let body_w = 3.93
  let body_l = 3.
  let body_thickness = 1.5
end

module A3144PrintConfig : Config = struct
  (* Very exaggerated leg thickness to account for print tolerances, ensuring the
   * exit hole is not shut. *)
  let leg_w = 0.6
  let leg_thickness = 0.85
  let leg_l = 20.
  let leg_spacing = 1.25
  let leg_bend = 4.
  let leg_z_offset = -0.2
  let merge_legs = true
  let body_w = 4.4
  let body_l = 3.4
  let body_thickness = 1.5
end

module Make (C : Config) : S = struct
  include C

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

  let body = Model.cube ~center:true (body_w, body_l, body_thickness)
  let scad = Model.union [ body; legs ] |> Model.translate (0., 0., body_thickness /. 2.)

  let sink depth =
    let f i = Model.translate (0., 0., Float.of_int i *. leg_thickness /. -2.) scad in
    List.init (Int.of_float (depth /. leg_w *. 2.) + 1) ~f
end
