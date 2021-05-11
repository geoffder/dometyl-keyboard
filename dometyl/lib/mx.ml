open! Base
open! Scad_ml

module HoleConfig : KeyHole.Config with type k = unit = struct
  type k = unit
  type spec = k KeyHole.Kind.t

  let spec = KeyHole.Kind.Mx ()
  let outer_w = 19.
  let inner_w = 14.
  let thickness = 4.

  let clip hole =
    let clip =
      Model.rotate
        (Math.pi /. 2., 0., 0.)
        (Model.cube ~center:true (5., thickness -. 1.3, 0.5))
      |> Model.translate (0., inner_w /. 2., -1.3)
    in
    Model.difference hole [ clip; Model.mirror (0, 1, 0) clip ]
end
