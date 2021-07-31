open! Base
open! Scad_ml

let inner_rad = 2.0
let outer_rad = 4.0
let foot_width = 2.0
let thickness = 4.0

let scad =
  let circ = Model.circle ~fn:16 outer_rad
  and hole = Model.circle ~fn:16 inner_rad
  and swoop =
    let x = Bezier.curve ~n_steps:10 (Bezier.quad ~p1:0. ~p2:foot_width ~p3:foot_width)
    and y = Bezier.curve ~n_steps:10 (Bezier.quad ~p1:0. ~p2:0. ~p3:inner_rad)
    and neg_diff = inner_rad -. outer_rad in
    let pts =
      (foot_width +. inner_rad, inner_rad)
      :: (foot_width +. outer_rad, neg_diff)
      :: (0., neg_diff)
      :: List.zip_exn x y
    in
    Model.polygon pts |> Model.translate (-.outer_rad -. foot_width, -.inner_rad, 0.)
  in
  Model.difference (Model.union [ circ; swoop; Model.mirror (1, 0, 0) swoop ]) [ hole ]
  |> Model.linear_extrude ~height:thickness
