open Base
open Scad_ml

type t = Model.t

let keys k1 k2 = Model.hull KeyHole.[ k1.faces.east.scad; k2.faces.west.scad ]
let joins j1 j2 = Model.hull Column.Join.[ j1.faces.east; j2.faces.west ]

let cols ~columns a_i b_i =
  let a : _ Column.t = Map.find_exn columns a_i
  and b = Map.find_exn columns b_i in
  let folder ~f ~key:_ ~data hulls =
    match data with
    | `Both (a', b') -> f a' b' :: hulls
    | _              -> hulls
  in
  Model.union
    (Map.fold2
       ~f:(folder ~f:keys)
       ~init:(Map.fold2 ~f:(folder ~f:joins) ~init:[] a.joins b.joins)
       a.keys
       b.keys )

let poly_bridge (face1 : KeyHole.Face.t) (face2 : KeyHole.Face.t) =
  (* NOTE: Same results as key_bridge when used like so. A scheme similar to
   * this could obviously be used for joining other parts, but I'll want to come
   * up with a way to provide a nice /reliable/ curve between them, instead of
   * a sharp cut. Alternative is a multipart hull with cylinders. *)
  let get_bez p1 p3 =
    let p2 = Vec3.(div (p1 <+> p3) (2., 2., 2.)) in
    Bezier.quad_vec3 ~p1 ~p2 ~p3
  in
  List.map2_exn
    ~f:get_bez
    (Points.to_clockwise_list face1.points)
    [ face2.points.top_right
    ; face2.points.top_left
    ; face2.points.bot_left
    ; face2.points.bot_right
    ]
  |> Bezier.prism_exn
