open! Base
open! Scad_ml

module type S = sig
  module Key : KeyHole.S

  type t = { scad : Model.t }

  val t : t
end

module Make (Key : KeyHole.S) : S = struct
  module Key = Key

  type t = { scad : Model.t }

  (* TODO: These should be config'd values, possibly in a Niz config record in
   * the Key module (requires this Key module instance to have it if so...) *)
  let notch_z = 4.
  let notch_height = 1.
  let arm_height = notch_z +. notch_height +. 1.
  let arm_radius = 2.
  (* let dome_thickness = 0.97 *)

  (* wall_height needs to be calculated with dome thickness, and how much of the housing
   * is going to be poking out of the plate.
   * TODO: I really to have these values available for coordinating between the top plate, and
   * this bottom piece. Best would be to make a model of the niz switch, then simply cut it out.
   * I also need to add a 45 degree chamfer up from the surface for the dome to fit under.
   * This way the bottom can be wider to accomodate the dome then come in for tight press fit
   * above it. *)
  (* let wall_height = 0. *)
  let base = Model.cube ~center:true (Key.outer_w, Key.outer_w, 2.)

  let arm =
    let pole =
      Model.difference
        (Model.circle arm_radius)
        [ Model.square (arm_radius *. 2.1, arm_radius *. 2.1)
          |> Model.translate (0., -.arm_radius, 0.)
        ]
      |> Model.linear_extrude ~height:arm_height
    in
    let cutter = Model.cube ~center:true (arm_radius, arm_radius *. 2., notch_height) in
    let notch = Model.translate (0., 0., notch_z +. (notch_height /. 2.)) cutter in
    let chamfer =
      Model.scale (1., 1., 2.) cutter
      |> Model.rotate (0., Math.pi /. 3., Math.pi)
      |> Model.translate (0., 0., arm_height +. 0.25)
    in
    Model.difference pole [ notch; chamfer ]

  let t =
    { scad =
        Model.union
          [ base
          ; Model.translate (18. /. -2., 0., 0.) (Model.rotate (0., 0., Math.pi) arm)
          ; Model.translate (18. /. 2., 0., 0.) arm
          ]
    }
end
