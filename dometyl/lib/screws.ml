open! Base
open! Scad_ml

let inner_rad = 2.0
let outer_rad = 4.0
let foot_width = 2.0
let thickness = 4.0
let rad_diff = outer_rad -. inner_rad

let scad =
  let circ = Model.circle ~fn:16 outer_rad
  and hole = Model.circle ~fn:16 inner_rad
  and swoop =
    let pts =
      (foot_width +. inner_rad, inner_rad)
      :: (foot_width +. outer_rad, -.rad_diff)
      :: (0., -.rad_diff)
      :: Bezier.curve
           ~n_steps:10
           (Bezier.quad_vec2
              ~p1:(0., 0.)
              ~p2:(foot_width, 0.)
              ~p3:(foot_width, inner_rad) )
    in
    Model.polygon pts |> Model.translate (-.outer_rad -. foot_width, -.inner_rad, 0.)
  in
  Model.difference (Model.union [ circ; swoop; Model.mirror (1, 0, 0) swoop ]) [ hole ]
  |> Model.linear_extrude ~height:thickness
