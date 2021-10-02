open Base
open Scad_ml
open Infix

module Steps = struct
  type t =
    [ `PerZ of float
    | `Flat of int
    ]

  let to_int t z =
    match t with
    | `PerZ mm -> Int.max 2 (Float.to_int (z /. mm))
    | `Flat n  -> n
end

module Edge = struct
  type t = float -> Vec3.t

  let translate p = Fn.compose (Vec3.add p)
  let mirror ax = Fn.compose (Vec3.mirror ax)
  let rotate r = Fn.compose (Vec3.rotate r)
  let rotate_about_pt r p = Fn.compose (Vec3.rotate_about_pt r p)

  let point_at_z ?(max_iter = 100) ?(tolerance = 0.001) t z =
    let bez_frac =
      Util.bisection_exn ~max_iter ~tolerance ~f:(fun s -> Vec3.get_z (t s) -. z) 0. 1.
    in
    t bez_frac
end

module EdgeDrawer = struct
  type drawer = Vec3.t -> Edge.t

  type t =
    { top : drawer
    ; bot : drawer
    }

  let make
      ?(max_iter = 100)
      ?(tolerance = 0.001)
      ~(get_bez : bool -> Vec3.t -> Edge.t)
      Points.{ top_left; top_right; bot_left; bot_right; _ }
    =
    let find_between lp rp (x, y, _) =
      let ((dx, dy, _) as diff) = Vec3.sub rp lp in
      let get_major, target =
        if Float.(abs dx > abs dy) then Vec3.get_x, x else Vec3.get_y, y
      in
      let ml = get_major lp
      and mr = get_major rp in
      if Float.(target > ml && target < mr) || Float.(target < ml && target > mr)
      then (
        let get s = Vec3.(add lp (mul_scalar diff s)) in
        let pos =
          Util.bisection_exn
            ~max_iter
            ~tolerance
            ~f:(fun s -> get_major (get s) -. target)
            0.
            1.
        in
        get pos )
      else if Float.(abs (target -. ml) < abs (target -. mr))
      then lp
      else rp
    in
    { top = find_between top_left top_right >> get_bez true
    ; bot = find_between bot_left bot_right >> get_bez false
    }

  let map ~f t = { top = f t.top; bot = f t.bot }
  let translate p = map ~f:(fun d start -> d start >> Vec3.add p)
  let mirror ax = map ~f:(fun d start -> d start >> Vec3.mirror ax)
  let rotate r = map ~f:(fun d start -> d start >> Vec3.rotate r)
  let rotate_about_pt r p = map ~f:(fun d start -> d start >> Vec3.rotate_about_pt r p)
end

module Edges = struct
  type t =
    { top_left : Edge.t
    ; top_right : Edge.t
    ; bot_left : Edge.t
    ; bot_right : Edge.t
    }

  let map ~f t =
    { top_left = f t.top_left
    ; top_right = f t.top_right
    ; bot_left = f t.bot_left
    ; bot_right = f t.bot_right
    }

  let translate p = map ~f:(Edge.translate p)
  let mirror ax = map ~f:(Edge.mirror ax)
  let rotate r = map ~f:(Edge.rotate r)
  let rotate_about_pt r p = map ~f:(Edge.rotate_about_pt r p)

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

type t =
  { scad : Scad.t
  ; start : Points.t
  ; foot : Points.t
  ; edge_drawer : EdgeDrawer.t
  ; edges : Edges.t
  ; screw : Eyelet.t option
  }

let translate p t =
  { scad = Scad.translate p t.scad
  ; start = Points.translate p t.start
  ; foot = Points.translate p t.foot
  ; edge_drawer = EdgeDrawer.translate p t.edge_drawer
  ; edges = Edges.translate p t.edges
  ; screw = Option.map ~f:(Eyelet.translate p) t.screw
  }

let mirror ax t =
  { scad = Scad.mirror ax t.scad
  ; start = Points.mirror ax t.start
  ; foot = Points.mirror ax t.foot
  ; edge_drawer = EdgeDrawer.mirror ax t.edge_drawer
  ; edges = Edges.mirror ax t.edges
  ; screw = Option.map ~f:(Eyelet.mirror ax) t.screw
  }

let rotate r t =
  { scad = Scad.rotate r t.scad
  ; start = Points.rotate r t.start
  ; foot = Points.rotate r t.foot
  ; edge_drawer = EdgeDrawer.rotate r t.edge_drawer
  ; edges = Edges.rotate r t.edges
  ; screw = Option.map ~f:(Eyelet.rotate r) t.screw
  }

let rotate_about_pt r p t =
  { scad = Scad.rotate_about_pt r p t.scad
  ; start = Points.rotate_about_pt r p t.start
  ; foot = Points.rotate_about_pt r p t.foot
  ; edge_drawer = EdgeDrawer.rotate_about_pt r p t.edge_drawer
  ; edges = Edges.rotate_about_pt r p t.edges
  ; screw = Option.map ~f:(Eyelet.rotate_about_pt r p) t.screw
  }

let swing_face ?(step = Float.pi /. 24.) key_origin face =
  let quat = Quaternion.make (KeyHole.Face.direction face)
  and free, pivot, rock_z, z_sign =
    let ortho = Vec3.(normalize (face.points.centre <-> key_origin)) in
    if Float.(Vec3.get_z ortho > 0.)
    then
      ( face.points.top_right
      , Vec3.(div (face.points.bot_left <+> face.points.bot_right) (-2., -2., -2.))
      , Vec3.get_z face.points.bot_right
      , 1. )
    else
      ( face.points.bot_right
      , Vec3.(div (face.points.top_left <+> face.points.top_right) (-2., -2., -2.))
      , Vec3.get_z face.points.top_right
      , -1. )
  in
  let diff a =
    Vec3.(get_z (Vec3.quaternion_about_pt (quat (a *. z_sign)) pivot free) -. rock_z)
    *. z_sign
  in
  let rec find_angle a last_diff =
    if Float.(a < pi)
    then (
      let d = diff a in
      if Float.(d < last_diff) then a -. step else find_angle (a +. step) d )
    else a -. step
  in
  let q = quat @@ (find_angle step (Vec3.get_z free -. rock_z) *. z_sign) in
  let face' = KeyHole.Face.quaternion_about_pt q pivot face in
  let ortho' =
    Vec3.quaternion_about_pt q pivot key_origin
    |> Vec3.sub face'.points.centre
    |> Vec3.normalize
  in
  face', ortho'

(* TODO: Think of scaling d1 based on how high the key is, though maybe should
 * do so in the higher level functions in walls that call this one. Having a larger
 * d1 value will improve the clearance for the tall columns, which aren't in such a
 * hurry to move in xy (since they have a larger distance to do it).
 *
 * NOTE: `Flat and `ZRatio as the type for d1? `ZRatio being a % of Z that should
 * be assigned as d1. Would that make the bow of the curve more consistent without
 * implementing and switching to splines?
   Update: Clearance not using d1 has been added, so this is more of a cosmetic
   consideration now. *)
let poly_siding
    ?(x_off = 0.)
    ?(y_off = 0.)
    ?(z_off = 0.)
    ?(clearance = 1.5)
    ?(n_steps = `Flat 4)
    ?(n_facets = 1)
    ?(d1 = 4.)
    ?(d2 = 7.)
    ?thickness
    ?eyelet_config
    side
    (key : _ KeyHole.t)
  =
  let start_face = KeyHole.Faces.face key.faces side
  and thickness = Option.value ~default:key.config.thickness thickness in
  let pivoted_face, ortho = swing_face key.origin start_face in
  let cleared_face =
    KeyHole.Face.translate (Vec3.map (( *. ) clearance) ortho) pivoted_face
  in
  let xy = Vec3.(normalize (mul ortho (1., 1., 0.)))
  (* NOTE: I think z_hop is much less relevant now, check what kind of values it
   * is getting, and consider removing. *)
  and z_hop = (Float.max 0. (Vec3.get_z ortho) *. key.config.thickness) +. z_off
  and top_offset =
    Vec3.(
      mul (1., 1., 0.) (cleared_face.points.bot_right <-> cleared_face.points.top_right))
  in
  let get_bez top ((x, y, z) as start) =
    let jog, d1, plus =
      let half_delta = (d2 -. d1) /. 2. in
      if top
      then thickness, d1 +. Float.max half_delta 0., top_offset
      else 0., d1 +. Float.min half_delta 0., (0., 0., 0.)
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
  let cw_points =
    let n = n_facets - 1 in
    Util.fill_points
      ~init:
        (Util.fill_points ~n cleared_face.points.bot_left cleared_face.points.bot_right)
      ~n
      cleared_face.points.top_right
      cleared_face.points.top_left
  in
  let corners =
    (* drop the extra non-corner elements for intermediate facets *)
    List.filteri ~f:(fun i _ ->
        i = 0 || i = n_facets || i = n_facets + 1 || i = 3 + ((n_facets - 1) * 2) )
  and steps =
    let adjust (_, _, z) =
      let lowest_z =
        let f m (_, _, z) = Float.min m z in
        Points.fold ~f ~init:Float.max_value cleared_face.points
      in
      Float.(to_int (z /. lowest_z *. of_int (Steps.to_int n_steps z)))
    in
    `Ragged (List.map ~f:adjust cw_points)
  and end_ps, bezs =
    List.foldi
      ~f:(fun i (ends, bs) p ->
        let e, b = get_bez (i > n_facets) p in
        e :: ends, b :: bs )
      ~init:([], [])
      (List.rev cw_points)
  in
  let foot = Points.of_clockwise_list_exn (corners end_ps) in
  let screw =
    let f config =
      let open Eyelet in
      let placement =
        let n = Vec3.negate xy in
        match config with
        | { hole = Through; _ } -> Normal n
        | { hole = Inset _; outer_rad; _ } ->
          let offset = outer_rad +. Vec3.(norm (sub foot.top_left foot.bot_left) /. 4.) in
          Point Vec3.(mean [ foot.top_left; foot.top_right ] <+> mul_scalar n offset)
      in
      make ~placement config foot.bot_left foot.bot_right
    in
    Option.map ~f eyelet_config
  in
  { scad =
      Scad.hull [ start_face.scad; cleared_face.scad ]
      :: Bezier.prism_exn bezs steps
      :: Option.value_map ~default:[] ~f:(fun s -> [ s.scad ]) screw
      |> Scad.union
      |> Fn.flip
           Scad.difference
           (Option.value_map ~default:[] ~f:(fun s -> Option.to_list s.cut) screw)
  ; start = start_face.points
  ; foot
  ; edge_drawer =
      EdgeDrawer.make
        ~get_bez:(fun top start -> snd (get_bez top start))
        cleared_face.points
  ; edges = Edges.of_clockwise_list_exn (corners bezs)
  ; screw
  }

let column_drop
    ?z_off
    ?clearance
    ?n_steps
    ?n_facets
    ?d1
    ?d2
    ?thickness
    ?eyelet_config
    ~spacing
    ~columns
    side
    idx
  =
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
  poly_siding
    ~x_off:(x_dodge *. -1.)
    ?z_off
    ?clearance
    ?d1
    ?d2
    ?thickness
    ?n_steps
    ?n_facets
    ?eyelet_config
    side
    key

let start_direction { start = { top_left; top_right; _ }; _ } =
  Vec3.normalize Vec3.(top_left <-> top_right)

let foot_direction { foot = { top_left; top_right; _ }; _ } =
  Vec3.normalize Vec3.(top_left <-> top_right)

let to_scad t = t.scad
