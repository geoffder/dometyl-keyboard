open Base
open Scad_ml

type t =
  { scad : Model.t
  ; points : Points.t
  }

let start_chunk side (k : _ KeyHole.t) =
  let rad = k.config.thickness /. 2. in
  let cyl =
    Model.cylinder ~center:true rad k.config.outer_w
    |> Model.rotate (0., Float.pi /. 2., 0.)
  and face = KeyHole.Faces.face k.faces side in
  let r = RotMatrix.(align_exn (KeyHole.Face.direction face) (1., 0., 0.) |> to_euler)
  and t = Vec3.(KeyHole.orthogonal k side <*> (rad, rad, rad)) in
  let centre = Vec3.(face.points.centre <+> t) in
  Model.rotate r cyl |> Model.translate centre, centre

let cyl_base_points ~thickness ~width ~direction ~ortho (x, y, _) =
  let centre = x, y, 0.
  and dir_step =
    Vec3.(map (( *. ) (width /. 2.)) (normalize (mul direction (1., 1., 0.))))
  and ortho_step =
    Vec3.(map (( *. ) (thickness /. 2.)) (normalize (mul ortho (1., 1., 0.))))
  in
  Points.
    { top_left = Vec3.(add centre (add dir_step ortho_step))
    ; top_right = Vec3.(add ortho_step (sub centre dir_step))
    ; bot_left = Vec3.(add dir_step (sub centre ortho_step))
    ; bot_right = Vec3.(sub centre (add dir_step ortho_step))
    ; centre
    }

let cyl_siding
    ?(x_off = 0.)
    ?(y_off = 0.)
    ?(z_off = 2.)
    ?(d1 = 4.)
    ?(d2 = 7.)
    ?(n_steps = 10)
    side
    (key : _ KeyHole.t)
  =
  let chunk, start = start_chunk side key
  and ortho = KeyHole.orthogonal key side
  and face = KeyHole.Faces.face key.faces side in
  let z_hop = (Float.max 0. (Vec3.get_z ortho) *. key.config.thickness) +. z_off in
  let t2 =
    Vec3.(
      mul (normalize (mul ortho (1., 1., 0.))) (d1, d1, 0.) |> add (x_off, y_off, z_hop))
  and t3 =
    (* Extend down until lowest point is at xy plane. Hull will close rest of gap. *)
    let lowest_z =
      let f m (_, _, z) = Float.min m z in
      Points.fold ~f ~init:Float.max_value face.points
    in
    Vec3.(
      add
        (mul (normalize (mul ortho (1., 1., 0.))) (d2, d2, 0.))
        (x_off, y_off, -.lowest_z))
  in
  let wall = Bezier.quad_hull ~t1:(0., 0., 0.) ~t2 ~t3 ~n_steps chunk
  and points =
    let KeyHole.{ thickness; outer_w = width; _ } = key.config in
    cyl_base_points
      ~thickness
      ~width
      ~direction:(KeyHole.Face.direction face)
      ~ortho
      (Vec3.add t3 start)
  in
  let scad =
    Model.union
      [ Model.hull [ chunk; face.scad ]
      ; wall
      ; Model.hull
          [ Model.translate t3 chunk
          ; Model.polygon
              (List.map
                 ~f:Vec3.to_vec2
                 [ points.top_left; points.top_right; points.bot_right; points.bot_left ] )
            |> Model.linear_extrude ~height:0.001
          ]
      ]
  in
  { scad; points }

let swing_face ?(step = Float.pi /. 24.) key_origin face =
  (* Iteratively find a rotation around it's bottom axis that brings face to a more
   * vertical orientation, returning the pivoted face and it's new orthogonal. *)
  let quat = Quaternion.make (KeyHole.Face.direction face)
  and pivot = Vec3.(div (face.points.bot_left <+> face.points.bot_right) (-2., -2., -2.))
  and top = face.points.top_right
  and bot_z = Vec3.get_z face.points.bot_right in
  let diff a =
    Vec3.(get_z Quaternion.(rotate_vec3_about_pt (quat a) pivot top) -. bot_z)
  in
  let rec find_angle a last_diff =
    if Float.(a < pi)
    then (
      let d = diff a in
      if Float.(d < last_diff) then a -. step else find_angle (a +. step) d )
    else a -. step
  in
  let q = quat @@ find_angle step (Vec3.get_z top -. bot_z) in
  let face' = KeyHole.Face.quaternion_about_pt q pivot face in
  let ortho' =
    Quaternion.rotate_vec3_about_pt q pivot key_origin
    |> Vec3.sub face.points.centre
    |> Vec3.normalize
  in
  face', ortho'

let poly_siding
    ?(x_off = 0.)
    ?(y_off = 0.)
    ?(z_off = 0.)
    ?(n_steps = 4)
    ?(d1 = 4.)
    ?(d2 = 7.)
    side
    (key : _ KeyHole.t)
  =
  let start_face = KeyHole.Faces.face key.faces side in
  let pivoted_face, ortho = swing_face key.origin start_face in
  let xy = Vec3.(normalize (mul ortho (1., 1., 0.))) in
  let z_hop = (Float.max 0. (Vec3.get_z ortho) *. key.config.thickness) +. z_off in
  let top_offset =
    Vec3.(
      mul (1., 1., 0.) (pivoted_face.points.bot_right <-> pivoted_face.points.top_right))
  in
  let get_bez top ((x, y, z) as p1) =
    let jog, d1, plus =
      if top
      then key.config.thickness, d1 +. ((d2 -. d1) /. 2.), top_offset
      else 0., d1, (0., 0., 0.)
    in
    let p2 =
      Vec3.(
        mul xy (d1 +. jog, d1 +. jog, 0.)
        |> add (x +. x_off, y +. y_off, z +. z_hop)
        |> add plus)
    and p3 =
      Vec3.(
        add (mul xy (d2 +. jog, d2 +. jog, 0.)) (x +. x_off, y +. y_off, 0.) |> add plus)
    in
    p3, Bezier.quad_vec3 ~p1 ~p2 ~p3
  in
  let cw_points = Points.to_clockwise_list pivoted_face.points in
  let steps =
    let lowest_z =
      let f m (_, _, z) = Float.min m z in
      Points.fold ~f ~init:Float.max_value pivoted_face.points
    in
    let adjust (_, _, z) = Float.(to_int (z /. lowest_z *. of_int n_steps)) in
    `Ragged (List.map ~f:adjust cw_points)
  and end_ps, bezs =
    List.foldi
      ~f:(fun i (ends, bs) p ->
        let e, b = get_bez (i > 1) p in
        e :: ends, b :: bs )
      ~init:([], [])
      (List.rev cw_points)
  in
  { scad =
      Model.union
        [ Model.hull [ start_face.scad; pivoted_face.scad ]; Bezier.prism_exn bezs steps ]
  ; points = Points.of_clockwise_list_exn end_ps
  }

let column_drop ?z_off ?d1 ?d2 ?n_steps ~spacing ~columns ?(kind = `Poly) side idx =
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
  match kind with
  | `Cyl  -> cyl_siding ~x_off:(x_dodge *. -1.) ?z_off ?d1 ?d2 ?n_steps side key
  | `Poly -> poly_siding ~x_off:(x_dodge *. -1.) ?z_off ?d1 ?d2 ?n_steps side key

let key_bridge k1 k2 = Model.hull KeyHole.[ k1.faces.east.scad; k2.faces.west.scad ]
let join_bridge j1 j2 = Model.hull Column.Join.[ j1.faces.east; j2.faces.west ]

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
