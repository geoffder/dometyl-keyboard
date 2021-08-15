open! Base
open! Scad_ml

type config =
  { outer_rad : float
  ; inner_rad : float
  ; thickness : float
  }

type t =
  { scad : Model.t
  ; centre : Vec3.t
  ; config : config
  }

let default_config = { outer_rad = 4.0; inner_rad = 2.0; thickness = 4.0 }
let m4_config = { outer_rad = 5.; inner_rad = 2.7; thickness = 4.0 }

let make ~normal ({ outer_rad; inner_rad; thickness } as config) p1 p2 =
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
  { scad =
      Model.difference (Model.union [ circ; swoop p1; swoop p2 ]) [ hole ]
      |> Model.linear_extrude ~height:thickness
  ; centre = hole_centre
  ; config
  }

let to_scad t = t.scad
