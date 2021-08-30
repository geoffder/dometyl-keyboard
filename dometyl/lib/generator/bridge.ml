open Base
open Scad_ml

type t = Model.t

let slide ?(d1 = 0.5) ?(d2 = 1.0) ~ortho scad =
  let a = Model.translate (Vec3.map (( *. ) d1) ortho) scad
  and b = Model.translate (Vec3.map (( *. ) d2) ortho) scad in
  Model.hull [ scad; a ], Model.hull [ a; b ]

let keys ?d1 ?d2 (k1 : _ KeyHole.t) (k2 : _ KeyHole.t) =
  if Float.(
       Vec3.get_z k1.faces.east.points.centre > Vec3.get_z k2.faces.west.points.centre)
  then (
    let a, b = slide ?d1 ?d2 ~ortho:(KeyHole.orthogonal k2 `West) k2.faces.west.scad in
    Model.union [ Model.hull [ k1.faces.east.scad; b ]; a ] )
  else (
    let a, b = slide ?d1 ?d2 ~ortho:(KeyHole.orthogonal k1 `East) k1.faces.east.scad in
    Model.union [ Model.hull [ k2.faces.west.scad; b ]; a ] )

let keys' (k1 : _ KeyHole.t) (k2 : _ KeyHole.t) =
  let slide side (k : _ KeyHole.t) =
    let ortho = KeyHole.orthogonal k side
    and face = KeyHole.Faces.face k.faces side in
    let a = Model.translate (Vec3.map (( *. ) 0.5) ortho) face.scad
    and b = Model.translate (Vec3.map (( *. ) 1.0) ortho) face.scad in
    Model.hull [ face.scad; a ], Model.hull [ a; b ]
  in
  let w1, w2 = slide `West k2 in
  let e1, e2 = slide `East k1 in
  Model.union [ w1; e1; Model.hull [ w2; e2 ] ]

let joins j1 j2 = Model.hull Column.Join.[ j1.faces.east; j2.faces.west ]

let cols ?d1 ?d2 ~columns a_i b_i =
  let a : _ Column.t = Map.find_exn columns a_i
  and b = Map.find_exn columns b_i
  and bookends keys join_idx =
    Map.find_exn keys join_idx, Map.find_exn keys (join_idx + 1)
  in
  let key_folder ~key:_ ~data hulls =
    match data with
    | `Both (a', b') -> keys ?d1 ?d2 a' b' :: hulls
    | _              -> hulls
  and join_folder ~key ~data hulls =
    match data with
    | `Both ((w_join : Column.Join.t), (e_join : Column.Join.t)) ->
      let w_last, w_next = bookends a.keys key
      and e_last, e_next = bookends b.keys key in
      let w_z =
        Vec3.(
          get_z
          @@ mean [ w_last.faces.east.points.centre; w_next.faces.east.points.centre ])
      and e_z =
        Vec3.(
          get_z
          @@ mean [ e_last.faces.west.points.centre; e_next.faces.west.points.centre ])
      in
      if Float.(w_z > e_z)
      then (
        let ortho =
          Vec3.(
            normalize (KeyHole.orthogonal e_last `West <+> KeyHole.orthogonal e_next `West))
        in
        let j1, j2 = slide ?d1 ?d2 ~ortho e_join.faces.west in
        j1 :: Model.hull [ w_join.faces.east; j2 ] :: hulls )
      else (
        let ortho =
          Vec3.(
            normalize (KeyHole.orthogonal w_last `East <+> KeyHole.orthogonal w_next `East))
        in
        let j1, j2 = slide ?d1 ?d2 ~ortho w_join.faces.east in
        j1 :: Model.hull [ e_join.faces.west; j2 ] :: hulls )
    | _ -> hulls
  in
  Model.union
    (Map.fold2
       ~f:key_folder
       ~init:(Map.fold2 ~f:join_folder ~init:[] a.joins b.joins)
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
