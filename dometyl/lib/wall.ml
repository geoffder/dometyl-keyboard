open Base
open Scad_ml

let bisection_exn ?(max_iter = 100) ~tolerance ~f lower upper =
  let rec loop i a b =
    let c = (a +. b) /. 2. in
    let res = f c in
    if Float.(res = 0. || (b -. a) /. 2. < tolerance)
    then c
    else if i < max_iter
    then
      if Float.(Sign.equal (sign_exn res) (sign_exn (f a)))
      then loop (i + 1) c b
      else loop (i + 1) a c
    else failwith "Maximum iterations reached in bisection search."
  in
  loop 0 lower upper

module Edge = struct
  type t = float -> Vec3.t

  let point_at_z ?(max_iter = 100) ?(tolerance = 0.001) t z =
    let bez_frac =
      bisection_exn ~max_iter ~tolerance ~f:(fun s -> Vec3.get_z (t s) -. z) 0. 1.
    in
    t bez_frac
end

module Edges = struct
  type t =
    { top_left : Edge.t
    ; top_right : Edge.t
    ; bot_left : Edge.t
    ; bot_right : Edge.t
    }

  let of_clockwise_list_exn = function
    | [ top_left; top_right; bot_right; bot_left ] ->
      { top_left; top_right; bot_left; bot_right }
    | _ -> failwith "Expect list of length 4, with edges beziers in clockwise order."

  let of_clockwise_list l =
    try Ok (of_clockwise_list_exn l) with
    | Failure e -> Error e

  let get t = function
    | `TL -> t.top_left
    | `TR -> t.top_right
    | `BL -> t.bot_left
    | `BR -> t.bot_right
end

(* TODO: store original keyface coordinates (important for joining walls for
 * closed designs, otherwise their are trought created by the fact that the edges
 * actually begin away from the wall on the "swung" face. *)
type t =
  { scad : Model.t
  ; start : Points.t
  ; foot : Points.t
  ; edges : Edges.t
  }

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
    |> Vec3.sub face'.points.centre
    |> Vec3.normalize
  in
  face', ortho'

(* TODO: add ability to specify steps per distance that wall will travel from the
 * key face as opposed to an absolute number of steps. This should help protect against
 * breakage when walls become too short, making the flat number of steps too great. *)
let poly_siding
    ?(x_off = 0.)
    ?(y_off = 0.)
    ?(z_off = 0.)
    ?(n_steps = 4)
    ?(d1 = 4.)
    ?(d2 = 7.)
    ?thickness
    side
    (key : _ KeyHole.t)
  =
  let start_face = KeyHole.Faces.face key.faces side
  and thickness = Option.value ~default:key.config.thickness thickness in
  let pivoted_face, ortho = swing_face key.origin start_face in
  let xy = Vec3.(normalize (mul ortho (1., 1., 0.))) in
  (* NOTE: I think z_hop is much less relevant now, check what kind of values it
   * is getting, and consider removing. *)
  let z_hop = (Float.max 0. (Vec3.get_z ortho) *. key.config.thickness) +. z_off in
  let top_offset =
    Vec3.(
      mul (1., 1., 0.) (pivoted_face.points.bot_right <-> pivoted_face.points.top_right))
  in
  let get_bez top ((x, y, z) as start) =
    let jog, d1, plus =
      if top then thickness, d1 +. ((d2 -. d1) /. 2.), top_offset else 0., d1, (0., 0., 0.)
    in
    let p1 = Vec3.(start <-> mul ortho (0.0002, 0.0002, 0.0002)) (* fudge for union *)
    and p2 =
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
  ; start = start_face.points
  ; foot = Points.of_clockwise_list_exn end_ps
  ; edges = Edges.of_clockwise_list_exn bezs
  }

let column_drop ?z_off ?d1 ?d2 ?thickness ?n_steps ~spacing ~columns side idx =
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
  poly_siding ~x_off:(x_dodge *. -1.) ?z_off ?d1 ?d2 ?thickness ?n_steps side key

let direction { foot = { top_left; top_right; _ }; _ } =
  Vec3.normalize Vec3.(top_left <-> top_right)
