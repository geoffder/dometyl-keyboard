open! Base
open! Scad_ml

(* TODO: Rather than returning just a scad from the connection functions, try using
   a record that includes the "outline points", e.g. the "top" or outside edge between
   the feet of the connected walls. Then, I can string those together to generate the
   base plate. I have not thought of another way yet that would allow the preservation
   of the peculiar concave shape while filling in the gaps. Hull would easily fill in
   the holes in the projection of the case, but the front inlet shape would be lost. *)

(* TODO: calculate multiple points up z, so that it lines up with the wall edge
   better. straight_base and join_walls does not use this helper, and
   inward_elbow_base uses a hybrid, so I will have to make the same upgrades there,
   and ensure that they remain compatible. Also, maybe try to improve the abstractions
   to not rely on the "is top?" lists that I am using (which will need to change if
   using a variable number of steps for the endpoints.) *)
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
    ?(bow_out = true)
    (w1 : Wall.t)
    (w2 : Wall.t)
  =
  let dir1 = Wall.foot_direction w1
  and dir2 = Wall.foot_direction w2
  and dist = d, d, 0.
  and width = Vec3.(norm (w1.foot.top_right <-> w1.foot.bot_right)) *. scale in
  let get_bez top start dest =
    let outward =
      if Bool.equal top bow_out then Vec3.add (width, width, 0.) dist else dist
    in
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
    ?(n_steps = 12)
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

let inward_elbow_base ?(height = 11.) ?(n_steps = 6) ?(d = 0.) (w1 : Wall.t) (w2 : Wall.t)
  =
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
    and p2 = Vec3.(start <+> mul mask diff <+> map (( *. ) d) dir2)
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

let straight_base
    ?(height = 11.)
    ?(fudge_factor = 6.)
    ?(min_width = 4.5)
    (w1 : Wall.t)
    (w2 : Wall.t)
  =
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
    let angle_extra =
      if Float.(abs minor_diff > abs major_diff)
      then Float.(abs (min (abs major_diff -. fudge_factor) 0.)) /. 2.
      else 0.
    in
    (* Check now thick the connection is in the middle, and create a rough adjustment
       if it is less than the specified minimum. *)
    let extra =
      if Float.(angle_extra = 0.)
      then (
        let mid_top = Vec3.mean [ w1.foot.top_right; w2.foot.top_left ]
        and mid_bot = Vec3.mean [ w1.foot.bot_right; w2.foot.bot_left ] in
        let thickness = Vec3.(norm (mid_top <-> mid_bot)) in
        if Float.(thickness < min_width) then min_width -. thickness else 0. )
      else angle_extra
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
  let starts og =
    let up_bot = Wall.Edge.point_at_z w1.edges.bot_right height in
    [ ( if og
      then w1.foot.bot_right
      else if not outward
      then fudge dir1 w1.foot.bot_right
      else fudge (Vec3.negate dir1) w1.foot.bot_right )
    ; w1.foot.top_right
    ; Wall.Edge.point_at_z w1.edges.top_right height
    ; ( if og
      then up_bot
      else if not outward
      then fudge dir1 up_bot
      else fudge (Vec3.negate dir1) up_bot )
    ]
    |> List.map ~f:Vec3.(add (mul dir1 (overlap, overlap, 0.)))
  and dests og =
    let up_bot = Wall.Edge.point_at_z w2.edges.bot_left height
    and slide = fudge (Vec3.negate dir2) in
    [ ( if og
      then w2.foot.bot_left
      else if outward
      then slide w2.foot.bot_left
      else fudge dir2 w2.foot.bot_left )
    ; w2.foot.top_left
    ; Wall.Edge.point_at_z w2.edges.top_left height
    ; (if og then up_bot else if outward then slide up_bot else fudge dir2 up_bot)
    ]
    |> List.map ~f:Vec3.(add (mul dir2 (-0.05, -0.05, 0.)))
  in
  Model.union
    [ Util.prism_exn (starts false) (dests false)
    ; ( if outward
      then Util.prism_exn (starts true) (starts false)
      else Util.prism_exn (dests false) (dests true) )
    ]

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
    if (not Float.(Sign.equal (sign_exn major_diff) (sign_exn major_ax)))
       && Float.(abs major_diff > abs minor_diff)
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
    ?min_straight_width
    ?n_steps
    ?join_steps
    ?(join_index = true)
    ?fudge_factor
    ?snake_d
    ?snake_scale
    ?cubic_d
    ?cubic_scale
    ?thumb_height
    ?(pinky_idx = 4)
    ?(close_thumb = false)
    ?(close_pinky = false)
    Walls.{ body; thumb }
  =
  (* TODO:
     - bez_base is not great on the northeast corner when there is actually a wall
       on that side that needs to be connected, because it was designed for cornering
       like it is used on the west side.
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
    let index =
      if join_index
      then join_walls ?n_steps:join_steps
      else straight_base ~height:index_height ?min_width:min_straight_width
    in
    List.init
      ~f:(fun i ->
        Option.map2
          ~f:
            (( if i = 0
             then index
             else if i >= 3 && close_pinky
             then join_walls ?n_steps:join_steps
             else straight_base ?height ?min_width:min_straight_width )
               ?fudge_factor )
          (col `N i)
          (col `N (i + 1)) )
      (n_cols - 1)
  in
  let east =
    let north = col `N (n_cols - 1)
    and south = col `S (n_cols - 1) in
    if Map.is_empty body.sides.east
    then
      Option.map2
        ~f:(cubic_base ?height ?d:cubic_d ?scale:cubic_scale ?n_steps)
        north
        south
    else (
      let draw_corner = Option.map2 ~f:(bez_base ?height ?n_steps) in
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
      then
        if close_pinky
        then join_walls ?n_steps:join_steps ?fudge_factor
        else inward_elbow_base ~d:1. ?height ?n_steps
      else straight_base ?height ?fudge_factor ?min_width:min_straight_width
    in
    List.init
      ~f:(fun i ->
        let idx = n_cols - 1 - i in
        Option.map2 ~f:(base idx) (col `S idx) (col `S (idx - 1)) )
      (n_cols - 1)
  in
  let sw_thumb, nw_thumb, ew_thumb, es_thumb, e_link, w_link =
    let Walls.Thumb.{ north = w_n; south = w_s } = Map.find_exn thumb.keys 0
    and _, Walls.Thumb.{ south = e_s; _ } = Map.max_elt_exn thumb.keys
    and corner = Option.map2 ~f:(join_walls ?n_steps:join_steps ~fudge_factor:0.)
    and link =
      Option.map2
        ~f:(snake_base ?height:thumb_height ?scale:snake_scale ?d:snake_d ?n_steps)
    in
    let sw = corner w_s thumb.sides.west
    and nw = corner thumb.sides.west w_n
    and ew =
      if close_thumb
      then (
        let _, scads =
          let join = join_walls ?n_steps:join_steps ?fudge_factor
          and get d = d.Walls.Thumb.south in
          Map.fold_right ~init:(None, []) ~f:(joiner ~get ~join) thumb.keys
        in
        Some (Model.union scads) )
      else Option.map2 ~f:(bez_base ?height ?n_steps) e_s w_s
    and es = corner thumb.sides.east e_s
    and e_link =
      if Option.is_some thumb.sides.east
      then Option.map2 ~f:(bez_base ~n_steps:3 ?height) (col `S 2) thumb.sides.east
      else link (col `S 2) e_s
    and w_link =
      Option.map2
        ~f:
          (cubic_base
             ?height:thumb_height
             ?scale:cubic_scale
             ?d:cubic_d
             ?n_steps
             ~bow_out:false )
        w_n
        (Map.find body.sides.west 0)
    in
    sw, nw, ew, es, e_link, w_link
  in
  west
  :: ( east
       :: sw_thumb
       :: nw_thumb
       :: ew_thumb
       :: es_thumb
       :: e_link
       :: w_link
       :: (north @ south)
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
