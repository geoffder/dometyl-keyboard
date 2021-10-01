open! Base
open! Scad_ml

type hole =
  | Through
  | Inset of float

type placement =
  | Normal of Vec3.t
  | Point of Vec3.t

type config =
  { outer_rad : float
  ; inner_rad : float
  ; thickness : float
  ; hole : hole
  }

type t =
  { scad : Scad.t
  ; cut : Scad.t option
  ; centre : Vec3.t
  ; config : config
  }

let translate p t =
  { t with scad = Scad.translate p t.scad; centre = Vec3.add p t.centre }

let mirror ax t =
  { t with scad = Scad.mirror ax t.scad; centre = Vec3.mirror ax t.centre }

let rotate r t = { t with scad = Scad.rotate r t.scad; centre = Vec3.rotate r t.centre }

let rotate_about_pt r p t =
  { t with
    scad = Scad.rotate_about_pt r p t.scad
  ; centre = Vec3.rotate_about_pt r p t.centre
  }

let default_config = { outer_rad = 4.0; inner_rad = 2.0; thickness = 4.0; hole = Through }
let m4_config = { outer_rad = 5.; inner_rad = 2.7; thickness = 4.0; hole = Through }
let bumpon_config = { outer_rad = 5.8; inner_rad = 5.; thickness = 2.; hole = Inset 0.5 }

let magnet_6x3_config =
  { outer_rad = 4.; inner_rad = 3.2; thickness = 4.; hole = Inset 3. }

let make
    ?(n_steps = 7)
    ~placement
    ({ outer_rad; inner_rad; thickness; hole } as config)
    p1
    p2
  =
  let base_centre = Vec3.(mul_scalar (p1 <+> p2) 0.5) in
  let hole_offset, foot_offset =
    match placement with
    | Normal n -> Vec3.map (( *. ) outer_rad) n, Vec3.mul_scalar n (-0.1)
    | Point p  ->
      let diff = Vec3.(p <-> base_centre) in
      diff, Vec3.mul_scalar (Vec3.normalize diff) (-0.1)
  in
  let hole_centre = Vec3.(base_centre <+> hole_offset) in
  let outer = Scad.circle ~fn:16 outer_rad |> Scad.translate hole_centre
  and inner = Scad.circle ~fn:16 inner_rad |> Scad.translate hole_centre
  and swoop p =
    let rad_offset = Vec3.(map (( *. ) outer_rad) (normalize (p <-> base_centre))) in
    hole_centre
    :: base_centre
    :: Vec3.add p foot_offset
    :: Bezier.curve
         ~n_steps
         (Bezier.quad_vec3
            ~p1:p
            ~p2:Vec3.(mean [ base_centre <+> rad_offset; p ])
            ~p3:Vec3.(hole_centre <+> rad_offset) )
    |> List.map ~f:Vec3.to_vec2
    |> Scad.polygon
  in
  let outline = Scad.union [ outer; swoop p1; swoop p2 ] in
  let scad, cut =
    match hole with
    | Through     ->
      Scad.difference outline [ inner ] |> Scad.linear_extrude ~height:thickness, None
    | Inset depth ->
      let inset = Scad.linear_extrude ~height:depth inner
      and foot = Scad.linear_extrude ~height:thickness outline in
      Scad.difference foot [ inset ], Some inset
  in
  { scad; cut; centre = hole_centre; config }

let to_scad t = t.scad
