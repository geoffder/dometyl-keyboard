open! Base
open! Scad_ml

let base_endpoints ~height hand (w : Wall.t) =
  let top, bot =
    match hand with
    | `Left  -> `TL, `BL
    | `Right -> `TR, `BR
  in
  [ Points.get w.foot bot
  ; Points.get w.foot top
  ; Wall.(Edge.point_at_z (Edges.get w.edges top) height)
  ; Wall.(Edge.point_at_z (Edges.get w.edges bot) height)
  ]

let base_steps ~n_steps starts dests =
  let norms = List.map2_exn ~f:(fun s d -> Vec3.(norm (s <-> d))) starts dests in
  let lowest_norm = List.fold ~init:Float.max_value ~f:Float.min norms in
  let adjust norm = Float.(to_int (norm /. lowest_norm *. of_int n_steps)) in
  `Ragged (List.map ~f:adjust norms)

let bez_base ?(height = 11.) ?(n_steps = 6) (w1 : Wall.t) (w2 : Wall.t) =
  let ((dx, dy, _) as dir1) = Wall.foot_direction w1
  and dir2 = Wall.foot_direction w2 in
  let mask = if Float.(abs dx > abs dy) then 1., 0., 0. else 0., 1., 0. in
  let get_bez start dest =
    let diff = Vec3.(dest <-> start) in
    let p1 = Vec3.(start <+> mul dir1 (0.01, 0.01, 0.)) (* fudge for union *)
    and p2 = Vec3.(start <+> mul mask diff)
    and p3 = Vec3.(dest <-> mul dir2 (0.01, 0.01, 0.)) in
    Bezier.quad_vec3 ~p1 ~p2 ~p3
  in
  let starts = base_endpoints ~height `Right w1 in
  let dests = base_endpoints ~height `Left w2 in
  let steps = base_steps ~n_steps starts dests
  and bezs = List.map2_exn ~f:get_bez starts dests in
  Bezier.prism_exn bezs steps

let cubic_base
    ?(height = 4.)
    ?(scale = 1.1)
    ?(d = 2.)
    ?(n_steps = 10)
    (w1 : Wall.t)
    (w2 : Wall.t)
  =
  let dir1 = Wall.foot_direction w1
  and dir2 = Wall.foot_direction w2
  and dist = d, d, 0.
  and width = Vec3.(norm (w1.foot.top_right <-> w1.foot.bot_right)) *. scale in
  let get_bez top start dest =
    let outward = if top then Vec3.add (width, width, 0.) dist else dist in
    let p1 = Vec3.(start <+> mul dir1 (0.01, 0.01, 0.)) (* fudge for union *)
    and p2 = Vec3.(start <-> mul dir1 outward)
    and p3 = Vec3.(dest <+> mul dir2 outward)
    and p4 = Vec3.(dest <-> mul dir2 (0.01, 0.01, 0.)) in
    Bezier.cubic_vec3 ~p1 ~p2 ~p3 ~p4
  in
  let starts = base_endpoints ~height `Right w1 in
  let dests = base_endpoints ~height `Left w2 in
  let steps = base_steps ~n_steps starts dests
  and bezs = List.map3_exn ~f:get_bez [ false; true; true; false ] starts dests in
  Bezier.prism_exn bezs steps

let snake_base
    ?(height = 4.)
    ?(scale = 1.5)
    ?(d = 2.)
    ?(n_steps = 10)
    (w1 : Wall.t)
    (w2 : Wall.t)
  =
  let dir1 = Wall.foot_direction w1
  and dir2 = Wall.foot_direction w2
  and dist = d, d, 0.
  and width = Vec3.(norm (w1.foot.top_right <-> w1.foot.bot_right)) *. scale in
  let get_bez top start dest =
    let outward = Vec3.add (width, width, 0.) dist in
    let p1 = Vec3.(start <+> mul dir1 (0.01, 0.01, 0.)) (* fudge for union *)
    and p2 = Vec3.(start <-> mul dir1 (if top then dist else outward))
    and p3 = Vec3.(dest <+> mul dir2 (if top then outward else dist))
    and p4 = Vec3.(dest <-> mul dir2 (0.01, 0.01, 0.)) in
    Bezier.cubic_vec3 ~p1 ~p2 ~p3 ~p4
  in
  let starts = base_endpoints ~height `Right w1 in
  let dests = base_endpoints ~height `Left w2 in
  let steps = base_steps ~n_steps starts dests
  and bezs = List.map3_exn ~f:get_bez [ false; true; true; false ] starts dests in
  Bezier.prism_exn bezs steps

let inward_elbow_base ?(height = 11.) ?(n_steps = 6) (w1 : Wall.t) (w2 : Wall.t) =
  (* Quad bezier, but starting from the bottom (inside face) of the wall and
   * projecting inward. This is so similar to bez_base that some generalization may
   * be possible to spare the duplication. Perhaps an option of whether the start is
   * the inward face (on the right) or the usual CW facing right side. *)
  let dir1 = Wall.foot_direction w1
  and dir2 = Wall.foot_direction w2
  and ((inx, iny, _) as inward_dir) =
    Vec3.normalize Vec3.(w1.foot.bot_right <-> w1.foot.top_right)
  in
  let mask = if Float.(abs inx > abs iny) then 1., 0., 0. else 0., 1., 0. in
  let get_bez start dest =
    let diff = Vec3.(dest <-> start) in
    let p1 = Vec3.(start <-> mul inward_dir (0.01, 0.01, 0.)) (* fudge for union *)
    and p2 = Vec3.(start <+> mul mask diff)
    and p3 = Vec3.(dest <-> mul dir2 (0.01, 0.01, 0.)) in
    Bezier.quad_vec3 ~p1 ~p2 ~p3
  in
  let starts =
    let up_bot = Wall.Edge.point_at_z w1.edges.bot_right height in
    let w = Vec3.(norm (w1.foot.bot_right <-> w1.foot.top_right)) in
    let slide p = Vec3.(add p (mul dir1 (w, w, 0.))) in
    [ slide w1.foot.bot_right; w1.foot.bot_right; up_bot; slide up_bot ]
  and dests = base_endpoints ~height `Left w2 in
  let steps = base_steps ~n_steps starts dests
  and bezs = List.map2_exn ~f:get_bez starts dests in
  Bezier.prism_exn bezs steps

let straight_base ?(height = 11.) ?(fudge_factor = 6.) (w1 : Wall.t) (w2 : Wall.t) =
  let ((dx, dy, _) as dir1) = Wall.foot_direction w1
  and dir2 = Wall.foot_direction w2 in
  let major_diff, minor_diff =
    let x, y, _ =
      Vec3.(
        mean [ w1.foot.bot_right; w1.foot.top_right ]
        <-> mean [ w2.foot.bot_left; w2.foot.top_left ])
    in
    if Float.(abs dx > abs dy) then x, y else y, x
  in
  let fudge d =
    (* For adjustment of bottom (inside face) points to account for steep angles
     * that would otherwise cause the polyhedron to fail. Distance moved is a
     * function of how far apart the walls are along the major axis of the first. *)
    let extra =
      if Float.(abs minor_diff > abs major_diff)
      then Float.(abs (min (abs major_diff -. fudge_factor) 0.))
      else 0.
    in
    Vec3.(add (mul d (extra, extra, 0.)))
  and overlap =
    let major_ax = if Float.(abs dx > abs dy) then dx else dy in
    if not Float.(Sign.equal (sign_exn major_diff) (sign_exn major_ax))
    then Float.abs major_diff
    else 0.01
  (* If the walls are overlapping, move back the start positions to counter. *)
  and outward =
    (* away from the centre of mass, or not? *)
    Float.(
      Vec3.(norm @@ (w1.foot.top_right <-> w2.foot.top_left))
      > Vec3.(norm @@ (w1.foot.top_right <-> w2.foot.bot_left)))
  in
  let starts =
    let up_bot = Wall.Edge.point_at_z w1.edges.bot_right height in
    [ (if not outward then fudge dir1 w1.foot.bot_right else w1.foot.bot_right)
    ; w1.foot.top_right
    ; Wall.Edge.point_at_z w1.edges.top_right height
    ; (if not outward then fudge dir1 up_bot else up_bot)
    ]
    |> List.map ~f:Vec3.(add (mul dir1 (overlap, overlap, 0.)))
  and dests =
    let up_top = Wall.Edge.point_at_z w2.edges.top_left height
    and up_bot = Wall.Edge.point_at_z w2.edges.bot_left height
    and slide = fudge (Vec3.negate dir2) in
    [ (if outward then slide w2.foot.bot_left else w2.foot.bot_left)
    ; w2.foot.top_left
    ; up_top
    ; (if outward then slide up_bot else up_bot)
    ]
    |> List.map ~f:Vec3.(add (mul dir2 (-0.05, -0.05, 0.)))
  in
  Util.prism_exn starts dests

let join_walls ?(n_steps = 6) ?(fudge_factor = 3.) (w1 : Wall.t) (w2 : Wall.t) =
  let ((dx, dy, _) as dir1) = Wall.foot_direction w1
  and dir2 = Wall.foot_direction w2 in
  let major_diff, minor_diff =
    let x, y, _ =
      Vec3.(
        mean [ w1.foot.bot_right; w1.foot.top_right ]
        <-> mean [ w2.foot.bot_left; w2.foot.top_left ])
    in
    if Float.(abs dx > abs dy) then x, y else y, x
  in
  (* Move the destination points along the outer face of the wall to improve angle. *)
  let fudge =
    let extra =
      if Float.(abs minor_diff > fudge_factor)
      then Float.(abs (min (abs major_diff -. fudge_factor) 0.))
      else 0.
    in
    Vec3.(add (mul dir2 (-.extra, -.extra, 0.)))
  and overlap =
    let major_ax = if Float.(abs dx > abs dy) then dx else dy in
    if not Float.(Sign.equal (sign_exn major_diff) (sign_exn major_ax))
    then Float.abs major_diff
    else 0.01
    (* If the walls are overlapping, move back the start positions to counter. *)
  in
  let starts =
    Bezier.curve_rev
      ~n_steps
      ~init:(Bezier.curve ~n_steps w1.edges.bot_right)
      w1.edges.top_right
    |> List.map ~f:Vec3.(add (mul dir1 (overlap, overlap, 0.)))
  and dests =
    Bezier.curve_rev ~n_steps ~init:(Bezier.curve ~n_steps w2.edges.bot_left) (fun step ->
        fudge @@ w2.edges.top_left step )
    |> List.map ~f:Vec3.(add (mul dir2 (-0.01, -0.01, 0.)))
  and wedge =
    (* Fill in the volume between the "wedge" hulls that are formed by swinging the
     * key face and moving it out for clearance prior to drawing the walls. *)
    Util.prism_exn
      (List.map
         ~f:Vec3.(add (mul (Wall.start_direction w1) (overlap, overlap, overlap)))
         [ w1.start.top_right
         ; w1.start.bot_right
         ; w1.edges.bot_right 0.
         ; w1.edges.top_right 0.0001
         ] )
      (List.map
         ~f:Vec3.(add (mul (Wall.start_direction w2) (-0.01, -0.01, -0.01)))
         [ w2.start.top_left
         ; w2.start.bot_left
         ; w2.edges.bot_left 0.
         ; fudge @@ w2.edges.top_left 0.0001
         ] )
  in
  Model.union [ Util.prism_exn starts dests; wedge ]

let joiner ~get ~join ~key:_ ~data (last, scads) =
  let next = get data in
  let scads' =
    match Option.map2 ~f:join last next with
    | Some j -> j :: scads
    | None   -> scads
  in
  Option.first_some next last, scads'

let skeleton
    ?(index_height = 11.)
    ?height
    ?n_steps
    ?fudge_factor
    ?snake_d
    ?snake_scale
    ?snake_height
    ?cubic_d
    ?(pinky_idx = 4)
    Walls.{ body; thumb }
  =
  (* TODO:
     - Test that this now allows walls on the sides other than the default skel
     - should still do some clean-up on this. *)
  let n_cols = Map.length body.cols
  and col side i =
    let c = Map.find_exn body.cols i in
    match side with
    | `N -> c.north
    | `S -> c.south
  and maybe_prepend opt l =
    match opt with
    | Some a -> a :: l
    | None   -> l
  in
  let west =
    let _, side =
      let f =
        joiner ~get:Option.some ~join:(straight_base ~height:index_height ?fudge_factor)
      in
      Map.fold ~init:(None, []) ~f body.sides.west
    and corner =
      Option.map2
        ~f:(bez_base ~height:index_height ?n_steps)
        (Option.map ~f:snd @@ Map.max_elt body.sides.west)
        (col `N 0)
    in
    Model.union (maybe_prepend corner side)
  in
  let north =
    let h i = if i = 0 then Some index_height else height in
    List.init
      ~f:(fun i ->
        Option.map2
          ~f:(straight_base ?height:(h i) ?fudge_factor)
          (col `N i)
          (col `N (i + 1)) )
      (n_cols - 1)
  in
  let east =
    let north = col `N (n_cols - 1)
    and south = col `S (n_cols - 1) in
    if Map.is_empty body.sides.east
    then Option.map2 ~f:(cubic_base ?height ?d:cubic_d ?n_steps) north south
    else (
      let draw_corner = Option.map2 ~f:(bez_base ~height:index_height ?n_steps) in
      let _, side =
        let f = joiner ~get:Option.some ~join:(straight_base ?height ?fudge_factor) in
        Map.fold_right ~init:(None, []) ~f body.sides.east
      and north_corner =
        draw_corner north (Option.map ~f:snd @@ Map.max_elt body.sides.east)
      and south_corner =
        draw_corner (Option.map ~f:snd @@ Map.min_elt body.sides.east) south
      in
      maybe_prepend north_corner side
      |> maybe_prepend south_corner
      |> Model.union
      |> Option.some )
  in
  let south =
    let base i =
      if i = pinky_idx
      then inward_elbow_base ?height ?n_steps
      else straight_base ?height ?fudge_factor
    in
    List.init
      ~f:(fun i ->
        let idx = n_cols - 1 - i in
        Option.map2 ~f:(base idx) (col `S idx) (col `S (idx - 1)) )
      (n_cols - 1)
  in
  let sw_thumb, nw_thumb, ew_thumb, e_link, w_link =
    let Walls.Thumb.{ north = w_n; south = w_s } = Map.find_exn thumb.keys 0
    and _, Walls.Thumb.{ south = e_s; _ } = Map.max_elt_exn thumb.keys
    and corner = Option.map2 ~f:(join_walls ?n_steps ~fudge_factor:0.)
    and link =
      Option.map2
        ~f:(snake_base ?height:snake_height ?scale:snake_scale ?d:snake_d ?n_steps)
    in
    let sw = corner w_s thumb.sides.west
    and nw = corner thumb.sides.west w_n
    and ew = Option.map2 ~f:(bez_base ?height ?n_steps) e_s w_s
    and e_link = link (col `S 2) e_s
    and w_link = link w_n (Map.find body.sides.west 0) in
    sw, nw, ew, e_link, w_link
  in
  west
  :: ( east :: sw_thumb :: nw_thumb :: ew_thumb :: e_link :: w_link :: (north @ south)
     |> List.filter_opt )
  |> Model.union

let closed ?n_steps ?fudge_factor Walls.{ body; thumb } =
  let n_cols = Map.length body.cols
  and col side i =
    let%bind.Option c = Map.find body.cols i in
    match side with
    | `N -> c.north
    | `S -> c.south
  and prepend_corner w1 w2 l =
    match Option.map2 ~f:(join_walls ?n_steps ~fudge_factor:0.) w1 w2 with
    | Some c -> c :: l
    | None   -> l
  in
  let _, sides =
    let f = joiner ~get:Option.some ~join:(join_walls ?n_steps ?fudge_factor) in
    let _, west = Map.fold ~init:(None, []) ~f body.sides.west in
    Map.fold_right ~init:(None, west) ~f body.sides.east
  in
  let _, sides_cols =
    let f side =
      joiner
        ~get:(Fn.flip Walls.Body.Cols.get @@ side)
        ~join:(join_walls ?n_steps ?fudge_factor)
    in
    let _, north = Map.fold ~init:(None, sides) ~f:(f `N) body.cols in
    Map.fold_right ~init:(None, north) ~f:(f `S) body.cols
  in
  let all_body =
    prepend_corner (Option.map ~f:snd (Map.max_elt body.sides.west)) (col `N 0) sides_cols
    |> prepend_corner
         (col `N (n_cols - 1))
         (Option.map ~f:snd (Map.max_elt body.sides.east))
    |> prepend_corner (Map.find body.sides.east 0) (col `S (n_cols - 1))
  in
  let all_walls =
    let _, scads =
      let join = join_walls ?n_steps ?fudge_factor
      and get d = d.Walls.Thumb.south in
      Map.fold_right ~init:(None, all_body) ~f:(joiner ~get ~join) thumb.keys
    in
    let southwest =
      Option.bind ~f:(fun (_, k) -> k.Walls.Thumb.south) (Map.min_elt thumb.keys)
    and northwest =
      Option.bind ~f:(fun (_, k) -> k.Walls.Thumb.north) (Map.min_elt thumb.keys)
    in
    prepend_corner
      thumb.sides.east
      (Option.bind ~f:(fun (_, k) -> k.Walls.Thumb.south) (Map.max_elt thumb.keys))
      scads
    |> prepend_corner southwest thumb.sides.west
    |> prepend_corner thumb.sides.west northwest
    |> prepend_corner northwest (Option.map ~f:snd @@ Map.min_elt body.sides.west)
  in
  all_walls |> Model.union
