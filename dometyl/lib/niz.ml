open! Base
open! Scad_ml

module Top = struct
  let clip_height = 1.35
end

module Bottom = struct
  let x = 17.15
  let y = 17.5
  let z = 4.
  let bulge_thickness = 0.5
  let bulge_length = 6.5
  let bulge_height = 3.2
  let ellipse_inset_x_rad = 1.5
  let circle_inset_y_scale = 1.2
  let corner_cut_rad = 5.
  let corner_cut_off = 2.75

  let ellipse =
    Model.scale (1., circle_inset_y_scale, 1.) (Model.circle ellipse_inset_x_rad)
    |> Model.translate (x /. 2., 0., 0.)

  let bulge =
    Model.cube (bulge_length, bulge_thickness, bulge_height)
    |> Model.translate (bulge_length /. -2., y /. 2., 0.)

  let cutter =
    Model.circle corner_cut_rad
    |> Model.translate ((x /. 2.) +. corner_cut_off, (y /. 2.) +. corner_cut_off, 0.)

  let scad =
    Model.difference
      (Model.square ~center:true (x, y))
      [ ellipse
      ; Model.mirror (1, 0, 0) ellipse
      ; cutter
      ; Model.mirror (1, 0, 0) cutter
      ; Model.mirror (1, 1, 0) cutter
      ; Model.mirror (0, 1, 0) cutter
      ]
    |> Model.linear_extrude ~height:z
    |> fun b -> Model.union [ b; bulge; Model.mirror (0, 1, 0) bulge ]
end

module HoleConfig : KeyHole.Config = struct
  let outer_w = 19.
  let inner_w = 14.
  let thickness = 4.

  let clip hole =
    let inset_depth = thickness -. Top.clip_height in
    let inset =
      Model.square ~center:true (inner_w +. 3.15, inner_w +. 3.5)
      |> Model.linear_extrude ~height:(inset_depth +. 0.1)
      |> Model.translate (0., 0., (thickness /. -2.) -. 0.1)
    in
    Model.union [ Model.difference hole [ inset ] ]
end
