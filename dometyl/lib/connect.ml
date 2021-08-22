open! Base
open! Scad_ml

(* TODO: cubic base for w_link of skeleton thumb is unreliable. Worked before,
   but with current rotation and closeness to the body it is faltering. *)
type t =
  { scad : Model.t
  ; outline : Vec3.t list
  ; inline : Vec3.t list
  }

let bounding_box { outline; _ } =
  let f (top, right, bot, left) (x, y, _) =
    let top' = Float.max top y
    and right' = Float.max right x
    and bot' = Float.min bot y
    and left' = Float.min left x in
    top', right', bot', left'
  in
  List.fold ~f ~init:Float.(min_value, min_value, max_value, max_value) outline

let centre (top, right, bot, left) = (left +. right) /. 2., (top +. bot) /. 2.

let translate p t =
  { scad = Model.translate p t.scad
  ; outline = List.map ~f:(Vec3.add p) t.outline
  ; inline = List.map ~f:(Vec3.add p) t.inline
  }

let rotate r t =
  { scad = Model.rotate r t.scad
  ; outline = List.map ~f:(Vec3.rotate r) t.outline
  ; inline = List.map ~f:(Vec3.rotate r) t.inline
  }

let rotate_about_pt r p t =
  { scad = Model.rotate_about_pt r p t.scad
  ; outline = List.map ~f:(Vec3.rotate_about_pt r p) t.outline
  ; inline = List.map ~f:(Vec3.rotate_about_pt r p) t.inline
  }

(* Assumes lists are lead with the outer (top) line along the xy plane. *)
let prism_connection bezs steps =
  let n_steps =
    match steps with
    | `Uniform n -> n
    | `Ragged ns -> List.hd_exn ns
  in
  { scad = Bezier.prism_exn bezs steps
  ; outline = Bezier.curve ~n_steps (List.hd_exn bezs)
  ; inline = Bezier.curve ~n_steps (List.last_exn bezs)
  }

let clockwise_union ts =
  let collect ~init line = List.fold ~init ~f:(fun ps p -> p :: ps) line in
  let f (scads, out_pts, in_pts) { scad; outline; inline } =
    scad :: scads, collect ~init:out_pts outline, collect ~init:in_pts inline
  in
  let scads, out_pts, in_pts = List.fold ~init:([], [], []) ~f ts in
  { scad = Model.union scads; outline = List.rev out_pts; inline = List.rev in_pts }

let outline_2d t = List.map ~f:Vec3.to_vec2 t.outline
let inline_2d t = List.map ~f:Vec3.to_vec2 t.inline

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
  [ Points.get w.foot top
  ; Wall.(Edge.point_at_z (Edges.get w.edges top) height)
  ; Wall.(Edge.point_at_z (Edges.get w.edges bot) height)
  ; Points.get w.foot bot
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
  prism_connection bezs steps

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
  and bezs = List.map3_exn ~f:get_bez [ true; true; false; false ] starts dests in
  prism_connection bezs steps

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
  and bezs = List.map3_exn ~f:get_bez [ true; true; false; false ] starts dests in
  prism_connection bezs steps

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
    [ w1.foot.bot_right; up_bot; slide up_bot; slide w1.foot.bot_right ]
  and dests = base_endpoints ~height `Left w2 in
  let steps = base_steps ~n_steps starts dests
  and bezs = List.map2_exn ~f:get_bez starts dests in
  let t = prism_connection bezs steps in
  { t with outline = w1.foot.top_right :: t.outline }

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
      Vec3.(norm (w1.foot.top_right <-> w2.foot.top_left))
      > Vec3.(norm (w1.foot.top_right <-> w2.foot.bot_left)))
  in
  (* The connector has two parts, and is drawn depending on whether it is going
     outward (away from plate). If moving inward, the straight move (along wall
     axis) occurs before turning towards the next wall, for outward, the
     opposite. `og` indicates whether the points include the true inner wall
     start/end positions, or are intermediaries. *)
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
  let extra_starts = starts false
  and extra_dests = dests false in
  { scad =
      Model.union
        [ Util.prism_exn extra_starts extra_dests
        ; ( if outward
          then Util.prism_exn (starts true) extra_starts
          else Util.prism_exn extra_dests (dests true) )
        ]
  ; outline = [ w1.foot.top_right; w2.foot.top_left ]
  ; inline =
      List.map
        ~f:List.hd_exn
        ( if outward
        then [ starts true; extra_starts; extra_dests ]
        else [ extra_starts; extra_dests; dests true ] )
  }

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
  (* Move the start or destination points along the outer face of the wall to improve angle. *)
  let outward =
    (* away from the centre of mass, or not? *)
    Float.(
      Vec3.(norm (w1.foot.top_right <-> w2.foot.top_left))
      > Vec3.(norm (w1.foot.top_right <-> w2.foot.bot_left)))
  in
  let fudge =
    let dir = if outward then Vec3.zero else dir2 in
    (* let dir = if outward then Vec3.negate dir1 else dir2 in *)
    let extra =
      if Float.(abs minor_diff > fudge_factor)
      then Float.(abs (min (abs major_diff -. fudge_factor) 0.))
      else 0.
    in
    Vec3.(add (mul dir (-.extra, -.extra, 0.)))
  and overlap =
    let major_ax = if Float.(abs dx > abs dy) then dx else dy in
    if (not Float.(Sign.equal (sign_exn major_diff) (sign_exn major_ax)))
       && Float.(abs major_diff > abs minor_diff)
    then Float.abs major_diff
    else 0.001
    (* If the walls are overlapping, move back the start positions to counter. *)
  in
  let starts =
    Bezier.curve_rev
      ~n_steps
      ~init:(Bezier.curve ~n_steps w1.edges.bot_right)
      (fun step -> (if outward then fudge else Fn.id) @@ w1.edges.top_right step)
    |> List.map ~f:Vec3.(add (mul dir1 (overlap, overlap, 0.)))
  and dests =
    Bezier.curve_rev ~n_steps ~init:(Bezier.curve ~n_steps w2.edges.bot_left) (fun step ->
        (if outward then Fn.id else fudge) @@ w2.edges.top_left step )
    |> List.map ~f:Vec3.(add (mul dir2 (-0.001, -0.001, 0.)))
  and wedge =
    (* Fill in the volume between the "wedge" hulls that are formed by swinging the
     * key face and moving it out for clearance prior to drawing the walls. *)
    Util.prism_exn
      (List.map
         ~f:Vec3.(add (mul (Wall.start_direction w1) (overlap, overlap, overlap)))
         [ w1.start.top_right
         ; w1.start.bot_right
         ; w1.edges.bot_right 0.
         ; (if outward then fudge else Fn.id) @@ w1.edges.top_right 0.0001
         ] )
      (List.map
         ~f:Vec3.(add (mul (Wall.start_direction w2) (-0.01, -0.01, -0.01)))
         [ w2.start.top_left
         ; w2.start.bot_left
         ; w2.edges.bot_left 0.
         ; (if outward then Fn.id else fudge) @@ w2.edges.top_left 0.0001
         ] )
  in
  { scad = Model.union [ Util.prism_exn starts dests; wedge ]
  ; outline =
      [ (if outward then fudge else Fn.id) w1.foot.top_right
      ; (if outward then Fn.id else fudge) w2.foot.top_left
      ]
  ; inline = [ w1.foot.bot_right; w2.foot.bot_left ]
  }

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
    ?thumb_cubic_d
    ?thumb_cubic_scale
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
     - should still do some clean-up on this, especially now that I ripped through here
       adjusting to the new type t. *)
  let n_cols = Map.length body.cols
  and col side i =
    let c = Map.find_exn body.cols i in
    match side with
    | `N -> c.north
    | `S -> c.south
  in
  let west =
    let corner =
      Option.map2
        ~f:(bez_base ~height:index_height ?n_steps)
        (Option.map ~f:snd @@ Map.max_elt body.sides.west)
        (col `N 0)
    in
    let _, side =
      let f =
        joiner ~get:Option.some ~join:(straight_base ~height:index_height ?fudge_factor)
      in
      Map.fold ~init:(None, []) ~f body.sides.west
    in
    List.rev @@ Util.prepend_opt corner side
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
    |> List.filter_opt
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
      |> Option.to_list
    else (
      let draw_corner = Option.map2 ~f:(bez_base ?height ?n_steps) in
      let south_corner =
        draw_corner (Option.map ~f:snd @@ Map.min_elt body.sides.east) south
      in
      let _, side =
        let f = joiner ~get:Option.some ~join:(straight_base ?height ?fudge_factor) in
        Map.fold_right ~init:(None, Option.to_list south_corner) ~f body.sides.east
      and north_corner =
        draw_corner north (Option.map ~f:snd @@ Map.max_elt body.sides.east)
      in
      Util.prepend_opt north_corner side )
  in
  let south =
    let base i =
      if i = pinky_idx
      then
        if close_pinky
        then join_walls ?n_steps:join_steps ?fudge_factor
        else inward_elbow_base ~d:1.5 ?height ?n_steps
      else straight_base ?height ?fudge_factor ?min_width:min_straight_width
    in
    List.init
      ~f:(fun i ->
        let idx = n_cols - 1 - i in
        Option.map2 ~f:(base idx) (col `S idx) (col `S (idx - 1)) )
      (n_cols - 1)
    |> List.filter_opt
  in
  let east_swoop, west_swoop =
    let Walls.Thumb.{ north = w_n; south = w_s } = Map.find_exn thumb.keys 0
    and _, Walls.Thumb.{ south = e_s; _ } = Map.max_elt_exn thumb.keys
    and corner = Option.map2 ~f:(join_walls ?n_steps:join_steps ~fudge_factor:0.)
    and link =
      Option.map2
        ~f:(snake_base ?height:thumb_height ?scale:snake_scale ?d:snake_d ?n_steps)
    in
    let es = corner thumb.sides.east e_s
    and e_link =
      if Option.is_some thumb.sides.east
      then Option.map2 ~f:(bez_base ~n_steps:3 ?height) (col `S 2) thumb.sides.east
      else link (col `S 2) e_s
    and sw = corner w_s thumb.sides.west
    and nw = corner thumb.sides.west w_n
    and w_link =
      Option.map2
        ~f:
          (cubic_base
             ?height:thumb_height
             ?scale:thumb_cubic_scale
             ?d:thumb_cubic_d
             ?n_steps
             ~bow_out:false )
        w_n
        (Map.find body.sides.west 0)
    in
    let east_swoop =
      if close_thumb
      then (
        let _, scads =
          let join = join_walls ?n_steps:join_steps ?fudge_factor
          and get d = d.Walls.Thumb.south in
          Map.fold_right
            ~init:(None, Util.prepend_opt es (Option.to_list e_link))
            ~f:(joiner ~get ~join)
            thumb.keys
        in
        List.rev scads )
      else Option.map2 ~f:(bez_base ?height ?n_steps) e_s w_s |> Option.to_list
    in
    east_swoop, List.filter_opt [ sw; nw; w_link ]
  in
  (* TODO: Do unions of each of these separately, then clockwise union them.
     Test whether breakage leads to just part of the connection "disappearing" on
     CGAL failure, rather than the whole thing. (make it easier to pin point what
     is actually failing.) *)
  List.join [ west; north; east; south; east_swoop; west_swoop ] |> clockwise_union

let closed ?n_steps ?fudge_factor Walls.{ body; thumb } =
  let n_cols = Map.length body.cols
  and col side i =
    let%bind.Option c = Map.find body.cols i in
    match side with
    | `N -> c.north
    | `S -> c.south
  and prepend_corner w1 w2 =
    Util.prepend_opt @@ Option.map2 ~f:(join_walls ?n_steps ~fudge_factor:0.) w1 w2
  in
  let east, west =
    let f = joiner ~get:Option.some ~join:(join_walls ?n_steps ?fudge_factor) in
    let _, west = Map.fold ~init:(None, []) ~f body.sides.west in
    let _, east = Map.fold_right ~init:(None, []) ~f body.sides.east in
    ( prepend_corner
        (col `N (n_cols - 1))
        (Option.map ~f:snd (Map.max_elt body.sides.east))
        (List.rev east)
    , List.rev
      @@ prepend_corner (Option.map ~f:snd (Map.max_elt body.sides.west)) (col `N 0) west
    )
  in
  let north, south =
    let f side =
      joiner
        ~get:(Fn.flip Walls.Body.Cols.get @@ side)
        ~join:(join_walls ?n_steps ?fudge_factor)
    in
    let _, north = Map.fold ~init:(None, []) ~f:(f `N) body.cols in
    let _, south = Map.fold_right ~init:(None, []) ~f:(f `S) body.cols in
    ( List.rev north
    , prepend_corner (Map.find body.sides.east 0) (col `S (n_cols - 1)) (List.rev south) )
  in
  let thumb =
    let _, south =
      let join = join_walls ?n_steps ?fudge_factor
      and get d = d.Walls.Thumb.south in
      Map.fold_right ~init:(None, []) ~f:(joiner ~get ~join) thumb.keys
    in
    let southwest =
      Option.bind ~f:(fun (_, k) -> k.Walls.Thumb.south) (Map.min_elt thumb.keys)
    and northwest =
      Option.bind ~f:(fun (_, k) -> k.Walls.Thumb.north) (Map.min_elt thumb.keys)
    and southeast =
      Option.bind ~f:(fun (_, k) -> k.Walls.Thumb.south) (Map.max_elt thumb.keys)
    in
    prepend_corner southwest thumb.sides.west south
    |> prepend_corner thumb.sides.west northwest
    |> prepend_corner northwest (Option.map ~f:snd @@ Map.min_elt body.sides.west)
    |> List.rev
    |> prepend_corner thumb.sides.east southeast
  in
  List.join [ west; north; east; south; thumb ] |> clockwise_union

let to_scad t = t.scad
