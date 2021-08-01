open! Base
open! Scad_ml

let inner_rad = 2.0
let outer_rad = 4.0
let foot_width = 2.0
let foot_base = 2.0
let foot_height = 2.0
let thickness = 4.0

(* let scad =
 *   let circ = Model.circle ~fn:16 outer_rad
 *   and hole = Model.circle ~fn:16 inner_rad
 *   and swoop =
 *     let pts =
 *       (foot_width +. outer_rad, foot_height)
 *       :: (foot_width +. outer_rad, -.foot_base)
 *       :: (0., -.foot_base)
 *       :: Bezier.curve
 *            ~n_steps:10
 *            (Bezier.quad_vec2
 *               ~p1:(0., 0.)
 *               ~p2:(foot_width, 0.)
 *               ~p3:(foot_width, foot_height) )
 *     in
 *     Model.polygon pts |> Model.translate (-.outer_rad -. foot_width, -.foot_height, 0.)
 *   in
 *   Model.difference (Model.union [ circ; swoop; Model.mirror (1, 0, 0) swoop ]) [ hole ]
 *   |> Model.linear_extrude ~height:thickness *)

let make ?(outer_rad = 4.0) ?(inner_rad = 2.0) ?(thickness = 2.0) ~normal p1 p2 =
  let base_centre = Vec3.(map (( *. ) 0.5) (p2 <+> p1))
  and hole_offset = Vec3.map (( *. ) outer_rad) normal
  and foot_offset = Vec3.map (( *. ) (-0.1)) normal in
  let hole_centre = Vec3.(base_centre <+> hole_offset) in
  let circ = Model.circle ~fn:16 outer_rad |> Model.translate hole_centre
  and hole = Model.circle ~fn:16 inner_rad |> Model.translate hole_centre
  and swoop p =
    let rad_offset = Vec3.(map (( *. ) outer_rad) (normalize (p <-> base_centre))) in
    hole_centre
    :: base_centre
    :: Vec3.add p foot_offset
    :: Bezier.curve
         ~n_steps:10
         (Bezier.quad_vec3
            ~p1:p
            ~p2:Vec3.(base_centre <+> rad_offset)
            ~p3:Vec3.(hole_centre <+> rad_offset) )
    |> List.map ~f:Vec3.to_vec2
    |> Model.polygon
  in
  Model.difference (Model.union [ circ; swoop p1; swoop p2 ]) [ hole ]
  |> Model.linear_extrude ~height:thickness

(* let scad = make ~normal:(0., 1., 0.) (-6., 0., 0.) (6., 0., 0.) *)
let scad = make ~normal:(Vec3.normalize (1., 1., 0.)) (-4.5, 4.5, 0.) (4.5, -4.5, 0.)
