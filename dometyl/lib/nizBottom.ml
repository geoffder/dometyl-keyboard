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
  let notch_height = Key.thickness /. 2.
  let arm_height = notch_z +. notch_height +. 1.
  let arm_radius = 1.5
  let base = Model.cube ~center:true (18., 18., 1.5)

  let arm =
    let pole =
      Model.difference
        (Model.circle arm_radius)
        [ Model.square (arm_radius *. 2.1, arm_radius *. 2.1)
          |> Model.translate (0., -.arm_radius, 0.)
        ]
      |> Model.linear_extrude ~height:arm_height
    in
    let notch =
      Model.cube ~center:true (arm_radius, arm_radius *. 2., notch_height)
      |> Model.translate (-.arm_radius, 0., notch_z +. (notch_height /. 2.))
    in
    Model.difference pole [ notch ]

  let t =
    { scad =
        Model.union
          [ base
          ; Model.translate (18. /. -2., 0., 0.) (Model.rotate (0., 0., Math.pi) arm)
          ; Model.translate (18. /. 2., 0., 0.) arm
          ]
    }
end
