open Base
open Scad_ml

let start_chunk side (k : _ KeyHole.t) =
  let cyl =
    Model.cylinder ~center:true (k.config.thickness /. 2.) k.config.outer_w
    |> Model.rotate (0., Float.pi /. 2., 0.)
  and face = KeyHole.Faces.face k.faces side in
  let r = RotMatrix.(align_exn (KeyHole.Face.direction face) (1., 0., 0.) |> to_euler)
  and t = Vec3.(KeyHole.orthogonal k side <*> (2., 2., 2.)) in
  let centre = Vec3.(face.points.centre <+> t) in
  Model.rotate r cyl |> Model.translate centre, centre

let siding
    ?(x_off = 0.)
    ?(y_off = 0.)
    ?(z_off = 2.)
    ?(d1 = 4.)
    ?(d2 = 7.)
    side
    (key : _ KeyHole.t)
  =
  let chunk, (x0, y0, z0) = start_chunk side key
  and chunk_rad = key.config.thickness /. 2.
  and ortho = KeyHole.orthogonal key side in
  let z_hop = (Float.max 0. (Vec3.get_z ortho) *. key.config.thickness) +. z_off in
  let t2 =
    Vec3.(
      mul (normalize (mul ortho (1., 1., 0.))) (d1, d1, 0.) |> add (x_off, y_off, z_hop))
  and t3 =
    Vec3.(
      add
        (mul (normalize (mul ortho (1., 1., 0.))) (d2, d2, 0.))
        (x_off, y_off, chunk_rad -. z0 -. (key.config.outer_w /. 2.)))
  in
  let face = KeyHole.Faces.face key.faces side in
  let wall = Bezier.quad_hull' ~t1:(0., 0., 0.) ~t2 ~t3 ~step:0.1 chunk in
  Model.difference
    (Model.union [ Model.hull [ chunk; face.scad ]; wall ])
    [ Model.cube
        ~center:true
        (key.config.outer_w *. 2., key.config.outer_w *. 2., key.config.outer_w *. 2.)
      |> Model.translate (x0, y0, -.key.config.outer_w)
    ]

let column_drop ?z_off ?d1 ?d2 ~spacing ~columns side idx =
  let key, face, hanging =
    let c : _ Column.t = Map.find_exn columns idx in
    match side with
    | `North ->
      let key = snd @@ Map.max_elt_exn c.keys in
      let edge_y = Vec3.get_y key.faces.north.points.centre in
      key, key.faces.north, Float.(( <= ) edge_y)
    | `South ->
      let key = Map.find_exn c.keys 0 in
      let edge_y = Vec3.get_y key.faces.south.points.centre in
      key, key.faces.south, Float.(( >= ) edge_y)
  in
  let x_dodge =
    match Map.find columns (idx + 1) with
    | Some next_c ->
      let right_x = Vec3.get_x face.points.top_right
      and next_face =
        KeyHole.Faces.face (snd @@ Map.max_elt_exn next_c.keys).faces side
      in
      let diff =
        if hanging (Vec3.get_y next_face.points.centre)
        then right_x -. Vec3.get_x next_face.points.bot_left
        else -.spacing
      in
      if Float.(diff > 0.) then diff +. spacing else Float.max 0. (spacing +. diff)
    | _           -> 0.
  in
  siding ~x_off:(x_dodge *. -1.) ?z_off ?d1 ?d2 side key

let poly_siding ?(z_up = 2.) side d1 d2 (key : _ KeyHole.t) =
  (* TODO: using very large bezier steps right now since the varying number of
   * points that it takes to get to the floor for eack side of the key when tented
   * above a certain amount causes problems for the polyhedron creation. Seems like
   * going down in z a similar amount for each side then cutting off below the
   * floor afterwards will be the simplest way to make sure the triangle mesh is
   * able to form well. *)
  let ortho = KeyHole.orthogonal key side in
  let z_hop = (Float.max 0. (Vec3.get_z ortho) *. key.config.thickness) +. z_up in
  let face = KeyHole.Faces.face key.faces side in
  let top_offset =
    Vec3.(mul (1., 1., 0.) (face.points.bot_right <-> face.points.top_right))
  in
  let get_bez top ((x, y, z) as p1) =
    let jog, d1 =
      if top then key.config.thickness, d1 +. ((d2 -. d1) /. 2.) else 0., d1
    in
    let xy = Vec3.(normalize (mul ortho (1., 1., 0.))) in
    let p2 =
      Vec3.(
        mul xy (d1 +. jog, d1 +. jog, 0.)
        |> add (x, y, z +. z_hop)
        |> add (if top then top_offset else 0., 0., 0.))
    and p3 =
      Vec3.(
        add (mul xy (d2 +. jog, d2 +. jog, 0.)) (x, y, 0.)
        |> add (if top then top_offset else 0., 0., 0.))
    in
    Bezier.quad_vec3 ~p1 ~p2 ~p3
  in
  Bezier.prism_exn
    [ get_bez true face.points.top_right
    ; get_bez true face.points.top_left
    ; get_bez false face.points.bot_left
    ; get_bez false face.points.bot_right
    ]
    0.25

let key_bridge k1 k2 = Model.hull KeyHole.[ k1.faces.east.scad; k2.faces.west.scad ]
let join_bridge j1 j2 = Model.hull Column.Join.[ j1.faces.east; j2.faces.west ]

let join_columns ~columns a_i b_i =
  let a : _ Column.t = Map.find_exn columns a_i
  and b = Map.find_exn columns b_i in
  let folder ~f ~key:_ ~data hulls =
    match data with
    | `Both (a', b') -> f a' b' :: hulls
    | _              -> hulls
  in
  Model.union
    (Map.fold2
       ~f:(folder ~f:key_bridge)
       ~init:(Map.fold2 ~f:(folder ~f:join_bridge) ~init:[] a.joins b.joins)
       a.keys
       b.keys )
