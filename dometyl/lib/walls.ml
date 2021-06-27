open Base
open Scad_ml

module Points = struct
  (* TODO: there is a lot of replication between this and the KeyHole.Face.Points
   * module. It's mainly the make which is different. Should amalgamate in own
   * file, with two different make functions (with adequate names). *)
  type t =
    { top_left : Vec3.t
    ; top_right : Vec3.t
    ; bot_left : Vec3.t
    ; bot_right : Vec3.t
    ; centre : Vec3.t
    }

  let make ~thickness ~width ~direction ~ortho (x, y, _) =
    let centre = x, y, 0.
    and dir_step =
      Vec3.(map (( *. ) (width /. 2.)) (normalize (mul direction (1., 1., 0.))))
    and ortho_step =
      Vec3.(map (( *. ) (thickness /. 2.)) (normalize (mul ortho (1., 1., 0.))))
    in
    { top_left = Vec3.(add centre (add dir_step ortho_step))
    ; top_right = Vec3.(add ortho_step (sub centre dir_step))
    ; bot_left = Vec3.(add dir_step (sub centre ortho_step))
    ; bot_right = Vec3.(sub centre (add dir_step ortho_step))
    ; centre
    }

  let map ~f t =
    { top_left = f t.top_left
    ; top_right = f t.top_right
    ; bot_left = f t.bot_left
    ; bot_right = f t.bot_right
    ; centre = f t.centre
    }

  let fold ~f ~init t =
    let flipped = Fn.flip f in
    f init t.top_left |> flipped t.top_right |> flipped t.bot_left |> flipped t.bot_right

  let translate p = map ~f:(Vec3.add p)
  let rotate r = map ~f:(Vec3.rotate r)
  let rotate_about_pt r p = map ~f:(Vec3.rotate_about_pt r p)

  let mark t =
    let f p = Model.cube ~center:true (1., 1., 1.) |> Model.translate p in
    Model.union [ f t.centre; f t.top_right; f t.top_left; f t.bot_right; f t.bot_left ]

  let of_clockwise_list_exn = function
    | [ top_left; top_right; bot_right; bot_left ] ->
      { top_left
      ; top_right
      ; bot_left
      ; bot_right
      ; centre =
          Vec3.(map (( *. ) 0.25) (top_left <+> top_right <+> bot_left <+> bot_right))
      }
    | _ -> failwith "Expect list of length 4, with corner points in clockwise order."

  let of_clockwise_list l =
    try Ok (of_clockwise_list_exn l) with
    | Failure e -> Error e
end

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
      KeyHole.Face.Points.fold ~f ~init:Float.max_value face.points
    in
    Vec3.(
      add
        (mul (normalize (mul ortho (1., 1., 0.))) (d2, d2, 0.))
        (x_off, y_off, -.lowest_z))
  in
  let wall = Bezier.quad_hull ~t1:(0., 0., 0.) ~t2 ~t3 ~n_steps chunk
  and points =
    let KeyHole.{ thickness; outer_w = width; _ } = key.config in
    Points.make
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

(* let find_angle ax top bot =
 *   let bot_z = Vec3.get_z bot in
 *   let step = Float.pi /. 24.
 *   and quat = Quaternion.make ax in
 *   let diff a = Vec3.(get_z Quaternion.(rotate_vec3 (quat a) top) -. bot_z) in
 *   let step = if Float.(diff (-.step) > diff step) then -.step else step in
 *   let rec loop a last_diff =
 *     if Float.(a < pi /. 2.)
 *     then (
 *       let d = diff a in
 *       if Float.(d < last_diff) then a -. step else loop (a +. step) d )
 *     else a -. step
 *   in
 *   loop step Float.min_value *)

let find_angle ax pivot top bot =
  let bot_z = Vec3.get_z bot in
  let step = Float.pi /. 24.
  and quat = Quaternion.make ax in
  let diff a =
    Vec3.(get_z Quaternion.(rotate_vec3_about_pt (quat a) top pivot) -. bot_z)
  in
  let rec loop a diffs =
    if Float.(a < pi)
    then loop (a +. step) ((a, diff a) :: diffs)
    else
      List.max_elt ~compare:(fun (_, d1) (_, d2) -> Float.compare d1 d2) diffs
      |> Option.value_map ~default:0. ~f:fst
  in
  loop step []

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
  (* TODO: Possible solution to make these more stable as curvature / number of rows
   * increase is to not start directly from the face, but rather from a new face
   * with an easier angle obtained by rotating it around the originating face's
   * directional axis and moving it out a bit. This could be with rotating and
   * translating the face and hulling to the original, or calculating a polyhedron.
   *
   * NOTE: Idea to try. If the z of the ortho is positive (like used in z_hop),
   * then take a point on the lower face edge (say the centre), add the thickness
   * in z, and move along the direction vector to get to either side, and draw a
   * face to hull to with polyhedron, or use the vector created by this theorhetical
   * face to calculate the rotation needed to match the key face to it.
   *
   * NOTE: Another possibility, extend from the top points along the ortho until
   * they would be directly above the lower point in z (can I calculate this regardless
   * of the orientation of the key though?) *)
  let ortho = KeyHole.orthogonal key side in
  let z_hop = (Float.max 0. (Vec3.get_z ortho) *. key.config.thickness) +. z_off in
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
        |> add (x +. x_off, y +. y_off, z +. z_hop)
        |> add (if top then top_offset else 0., 0., 0.))
    and p3 =
      Vec3.(
        add (mul xy (d2 +. jog, d2 +. jog, 0.)) (x +. x_off, y +. y_off, 0.)
        |> add (if top then top_offset else 0., 0., 0.))
    in
    p3, Bezier.quad_vec3 ~p1 ~p2 ~p3
  in
  let cw_points = KeyHole.Face.Points.to_clockwise_list face.points in
  let steps =
    let lowest_z =
      let f m (_, _, z) = Float.min m z in
      KeyHole.Face.Points.fold ~f ~init:Float.max_value face.points
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
  let dir = KeyHole.Face.direction face in
  (* let middle_bot =
   *   Vec3.(div (face.points.bot_left <+> face.points.bot_right) (2., 2., 2.))
   * in
   * let middle_above = Vec3.(middle_bot <+> (0., 0., key.config.thickness)) in
   * let left_corner =
   *   Vec3.(middle_above <+> map (( *. ) (-0.5 *. key.config.outer_w)) dir)
   * in
   * let right_corner =
   *   Vec3.(middle_above <+> map (( *. ) (0.5 *. key.config.outer_w)) dir)
   * in *)
  (* let left_corner = Vec3.(face.points.bot_left <+> (0., 0., key.config.thickness)) in
   * let right_corner = Vec3.(face.points.bot_right <+> (0., 0., key.config.thickness)) in
   * let mark t = Model.cube ~center:true (2., 2., 2.) |> Model.translate t in
   * { scad =
   *     Model.union [ Bezier.prism_exn bezs steps; mark left_corner; mark right_corner ]
   * ; points = Points.of_clockwise_list_exn end_ps
   * } *)
  let mark t = Model.cube ~center:true (2., 2., 2.) |> Model.translate t in
  let pivot =
    Vec3.(div (face.points.bot_left <+> face.points.bot_right) (-2., -2., -2.))
  in
  let a = find_angle dir pivot face.points.top_right face.points.bot_right in
  let q = Quaternion.make dir a in
  let rot_face = Model.quaternion_about_pt q pivot face.scad in
  { scad =
      Model.union
        [ Bezier.prism_exn bezs steps |> Model.translate (0., 0., 10.)
        ; rot_face
        ; Quaternion.rotate_vec3_about_pt q face.points.top_right pivot |> mark
        ]
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
    (KeyHole.Face.Points.to_clockwise_list face1.points)
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
