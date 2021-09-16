open! Base
open! Scad_ml
open! Infix

(* TODO: cubic base for w_link of skeleton thumb is unreliable. Worked before,
   but with current rotation and closeness to the body it is faltering. *)
(* TODO: related to jank fix of allowing switch to straight base rather than cubic
  for the w_link:
  - should I make config types for each of the connections?
  - would allow giving sum types holding the configs for some of the places
    where options for the type of connection are desirable.
  - this way I can cut down on the enormous number of separate parameters (some
    of which are ignored.)
 *)
type t =
  { scad : Model.t
  ; outline : Vec3.t list
  ; inline : Vec3.t list
  }

let centre (top, right, bot, left) = (left +. right) /. 2., (top +. bot) /. 2.

let translate p t =
  { scad = Model.translate p t.scad
  ; outline = List.map ~f:(Vec3.add p) t.outline
  ; inline = List.map ~f:(Vec3.add p) t.inline
  }

let mirror ax t =
  { scad = Model.mirror ax t.scad
  ; outline = List.map ~f:(Vec3.mirror ax) t.outline
  ; inline = List.map ~f:(Vec3.mirror ax) t.inline
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

let facet_points ?(rev = false) ?(n_facets = 1) ?(init = []) ~height bez =
  let step = height /. Float.of_int n_facets
  and start, continue, next =
    if rev then 1, ( >= ) n_facets, ( + ) 1 else n_facets, ( < ) 0, ( + ) (-1)
  in
  let rec loop ps i =
    if continue i
    then loop (Wall.Edge.point_at_z bez (Float.of_int i *. step) :: ps) (next i)
    else ps
  in
  loop init start

let endpoints ?n_facets ~height top top_bez bot_bez bot =
  top
  :: facet_points
       ?n_facets
       ~height
       ~init:(facet_points ~rev:true ?n_facets ~height ~init:[ bot ] bot_bez)
       top_bez

let base_endpoints ?(n_facets = 1) ~height hand (w : Wall.t) =
  let top, bot =
    match hand with
    | `Left  -> `TL, `BL
    | `Right -> `TR, `BR
  in
  endpoints
    ~n_facets
    ~height
    (Points.get w.foot top)
    (Wall.Edges.get w.edges top)
    (Wall.Edges.get w.edges bot)
    (Points.get w.foot bot)

let base_steps ~n_steps starts dests =
  let norms = List.map2_exn ~f:Vec3.distance starts dests in
  let lowest_norm = List.fold ~init:Float.max_value ~f:Float.min norms in
  let adjust norm = Float.(to_int (norm /. lowest_norm *. of_int n_steps)) in
  `Ragged (List.map ~f:adjust norms)

let bez_base ?(n_facets = 1) ?(height = 11.) ?(n_steps = 6) (w1 : Wall.t) (w2 : Wall.t) =
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
  let starts = base_endpoints ~n_facets ~height `Right w1 in
  let dests = base_endpoints ~n_facets ~height `Left w2 in
  let steps = base_steps ~n_steps starts dests
  and bezs = List.map2_exn ~f:get_bez starts dests in
  prism_connection bezs steps

let cubic_base
    ?(n_facets = 1)
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
  and width = Vec3.distance w1.foot.top_right w1.foot.bot_right *. scale in
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
  let starts = base_endpoints ~n_facets ~height `Right w1 in
  let dests = base_endpoints ~n_facets ~height `Left w2 in
  let steps = base_steps ~n_steps starts dests
  and bezs =
    List.fold2_exn
      ~init:(0, [])
      ~f:(fun (i, bs) s d -> i + 1, get_bez (i <= n_facets) s d :: bs)
      starts
      dests
    |> snd
    |> List.rev
  in
  prism_connection bezs steps

let snake_base
    ?(n_facets = 1)
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
  and width = Vec3.distance w1.foot.top_right w1.foot.bot_right *. scale in
  let get_bez top start dest =
    let outward = Vec3.add (width, width, 0.) dist in
    let p1 = Vec3.(start <+> mul dir1 (0.01, 0.01, 0.)) (* fudge for union *)
    and p2 = Vec3.(start <-> mul dir1 (if top then dist else outward))
    and p3 = Vec3.(dest <+> mul dir2 (if top then outward else dist))
    and p4 = Vec3.(dest <-> mul dir2 (0.01, 0.01, 0.)) in
    Bezier.cubic_vec3 ~p1 ~p2 ~p3 ~p4
  in
  let starts = base_endpoints ~n_facets ~height `Right w1 in
  let dests = base_endpoints ~n_facets ~height `Left w2 in
  let steps = base_steps ~n_steps starts dests
  and bezs =
    List.fold2_exn
      ~init:(0, [])
      ~f:(fun (i, bs) s d -> i + 1, get_bez (i <= n_facets) s d :: bs)
      starts
      dests
    |> snd
    |> List.rev
  in
  prism_connection bezs steps

let inward_elbow_base
    ?(n_facets = 1)
    ?(height = 11.)
    ?(n_steps = 6)
    ?(d = 0.)
    (w1 : Wall.t)
    (w2 : Wall.t)
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
    let w = Vec3.distance w1.foot.bot_right w1.foot.top_right in
    let slide p = Vec3.(add p (mul dir1 (w, w, 0.))) in
    endpoints
      ~n_facets
      ~height
      w1.foot.bot_right
      w1.edges.bot_right
      (w1.edges.bot_right >> slide)
      (slide w1.foot.bot_right)
  and dests = base_endpoints ~n_facets ~height `Left w2 in
  let steps = base_steps ~n_steps starts dests
  and bezs = List.map2_exn ~f:get_bez starts dests in
  let t = prism_connection bezs steps in
  { t with outline = w1.foot.top_right :: t.outline }

let straight_base
    ?(n_facets = 1)
    ?(height = 11.)
    ?(fudge_factor = 6.)
    ?(overlap_factor = 1.2)
    ?(min_width = 4.5)
    (w1 : Wall.t)
    (w2 : Wall.t)
  =
  let ((dx, dy, _) as dir1) = Wall.foot_direction w1
  and dir2 = Wall.foot_direction w2
  and ((lx, ly, _) as line) =
    Vec3.(
      mean [ w1.foot.bot_right; w1.foot.top_right ]
      <-> mean [ w2.foot.bot_left; w2.foot.top_left ])
  in
  let major_diff, minor_diff = if Float.(abs dx > abs dy) then lx, ly else ly, lx in
  let fudge d =
    (* For adjustment of bottom (inside face) points to account for steep angles
     * that would otherwise cause the polyhedron to fail. Distance moved is a
     * function of how far apart the walls are along the major axis of the first. *)
    let angle_extra =
      if Float.(abs minor_diff > abs major_diff)
      then Float.(abs (min (abs major_diff -. fudge_factor) 0.)) /. 2.
      else 0.
    and width_extra =
      (* Check now thick the connection is in the middle, and create a rough adjustment
       if it is less than the specified minimum. *)
      let mid_top = Vec3.mean [ w1.foot.top_right; w2.foot.top_left ]
      and mid_bot = Vec3.mean [ w1.foot.bot_right; w2.foot.bot_left ] in
      let mid_diff = Vec3.(mid_top <-> mid_bot) in
      let align = 1. -. Vec3.(norm @@ cross (normalize mid_diff) (normalize line)) in
      let thickness =
        Float.max
          0.
          Vec3.(norm mid_diff -. (distance w1.foot.top_right w1.foot.bot_right *. align))
      in
      if Float.(thickness < min_width) then min_width -. thickness else 0.
    in
    let extra = Float.max angle_extra width_extra in
    Vec3.(add (mul d (extra, extra, 0.)))
  and overlap =
    let major_ax = if Float.(abs dx > abs dy) then dx else dy
    and intersect = Points.overlapping_bounds w1.foot w2.foot in
    if Float.(
         ( (not (Sign.equal (sign_exn major_diff) (sign_exn major_ax)))
         && abs major_diff > abs minor_diff )
         || intersect > 0.)
    then (
      let rough_area =
        Vec3.(
          distance w1.foot.top_right w1.foot.top_left
          *. distance w1.foot.top_right w1.foot.bot_right)
      in
      Float.max (Float.abs major_diff) (intersect *. rough_area) *. overlap_factor )
    else 0.01
  (* If the walls are overlapping, move back the start positions to counter. *)
  and outward =
    (* away from the centre of mass, or not? *)
    Float.(
      Vec3.distance w1.foot.top_right w2.foot.top_left
      > Vec3.distance w1.foot.top_right w2.foot.bot_left)
  in
  (* The connector has two parts, and is drawn depending on whether it is going
     outward (away from plate). If moving inward, the straight move (along wall
     axis) occurs before turning towards the next wall, for outward, the
     opposite. `og` indicates whether the points include the true inner wall
     start/end positions, or are intermediaries. *)
  let starts og =
    let bot_bez =
      if og
      then w1.edges.bot_right
      else if not outward
      then w1.edges.bot_right >> fudge dir1
      else w1.edges.bot_right >> fudge (Vec3.negate dir1)
    and bot_pt =
      if og
      then w1.foot.bot_right
      else if not outward
      then fudge dir1 w1.foot.bot_right
      else fudge (Vec3.negate dir1) w1.foot.bot_right
    in
    endpoints ~n_facets ~height w1.foot.top_right w1.edges.top_right bot_bez bot_pt
    |> List.map ~f:Vec3.(add (mul dir1 (overlap, overlap, 0.)))
  and dests og =
    let slide = fudge (Vec3.negate dir2) in
    let bot_bez =
      if og
      then w2.edges.bot_left
      else if outward
      then w2.edges.bot_left >> slide
      else w2.edges.bot_left >> fudge dir2
    and bot_pt =
      if og
      then w2.foot.bot_left
      else if not outward
      then fudge dir2 w2.foot.bot_left
      else fudge (Vec3.negate dir2) w2.foot.bot_left
    in
    endpoints ~n_facets ~height w2.foot.top_left w2.edges.top_left bot_bez bot_pt
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
        ~f:List.last_exn
        ( if outward
        then [ starts true; extra_starts; extra_dests ]
        else [ extra_starts; extra_dests; dests true ] )
  }

let join_walls
    ?(n_steps = 6)
    ?(fudge_factor = 3.)
    ?(overlap_factor = 1.2)
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
  (* Move the start or destination points along the outer face of the wall to improve angle. *)
  let outward =
    (* away from the centre of mass, or not? *)
    Float.(
      Vec3.distance w1.foot.top_right w2.foot.top_left
      > Vec3.distance w1.foot.top_right w2.foot.bot_left)
  in
  let fudge =
    let dir = if outward then Vec3.zero else dir2 in
    let extra =
      if Float.(Vec3.(get_z w1.start.top_right > get_z w2.start.top_left))
      then Float.(abs (max (fudge_factor -. major_diff) 0.))
      else 0.
    in
    Vec3.(add (mul dir (-.extra, -.extra, 0.)))
  and overlap =
    let major_ax = if Float.(abs dx > abs dy) then dx else dy
    and intersect = Points.overlapping_bounds w1.foot w2.foot in
    if Float.(
         ( (not (Sign.equal (sign_exn major_diff) (sign_exn major_ax)))
         && abs major_diff > abs minor_diff )
         || intersect > 0.)
    then (
      let rough_area =
        Vec3.(
          distance w1.foot.top_right w1.foot.top_left
          *. distance w1.foot.top_right w1.foot.bot_right)
      in
      Float.max (Float.abs major_diff) (intersect *. rough_area) *. overlap_factor )
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
    |> List.map ~f:Vec3.(add (mul dir2 (-.overlap, -.overlap, 0.)))
  and wedge =
    (* Fill in the volume between the "wedge" hulls that are formed by swinging the
     * key face and moving it out for clearance prior to drawing the walls. *)
    Util.prism_exn
      (List.map
         ~f:Vec3.(add (mul (Wall.start_direction w1) (overlap, overlap, 0.)))
         [ w1.start.top_right
         ; w1.start.bot_right
         ; w1.edges.bot_right 0.
         ; (if outward then fudge else Fn.id) @@ w1.edges.top_right 0.0001
         ] )
      (List.map
         ~f:Vec3.(add (mul (Wall.start_direction w2) (-0.01, -0.01, 0.)))
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
  ; inline =
      [ Vec3.(add (mul dir1 (overlap, overlap, 0.)) w1.foot.bot_right)
      ; Vec3.(add (mul dir2 (-.overlap, -.overlap, 0.)) w2.foot.bot_left)
      ]
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
    ?(n_facets = 1)
    ?(index_height = 11.)
    ?height
    ?min_straight_width
    ?n_steps
    ?body_join_steps
    ?thumb_join_steps
    ?fudge_factor
    ?join_fudge_factor
    ?overlap_factor
    ?snake_d
    ?snake_scale
    ?cubic_d
    ?cubic_scale
    ?(west_link_cubic = true)
    ?thumb_cubic_d
    ?thumb_cubic_scale
    ?thumb_height
    ?(north_joins = fun i -> i < 2)
    ?(south_joins = fun _ -> false)
    ?(pinky_idx = 4)
    ?(pinky_elbow = true)
    ?(close_thumb = false)
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
        joiner
          ~get:Option.some
          ~join:
            (straight_base
               ~n_facets
               ~height:index_height
               ?fudge_factor
               ?overlap_factor
               ?min_width:min_straight_width )
      in
      Map.fold ~init:(None, []) ~f body.sides.west
    in
    List.rev @@ Util.prepend_opt corner side
  in
  let north =
    List.init
      ~f:(fun i ->
        Option.map2
          ~f:
            ( if north_joins i
            then
              join_walls
                ?n_steps:body_join_steps
                ?fudge_factor:join_fudge_factor
                ?overlap_factor
            else
              straight_base
                ~n_facets
                ?height:(if i < 2 then Some index_height else height)
                ?min_width:min_straight_width
                ?fudge_factor
                ?overlap_factor )
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
        ~f:(cubic_base ~n_facets ?height ?d:cubic_d ?scale:cubic_scale ?n_steps)
        north
        south
      |> Option.to_list
    else (
      let draw_corner = Option.map2 ~f:(bez_base ?height ?n_steps) in
      let south_corner =
        draw_corner (Option.map ~f:snd @@ Map.min_elt body.sides.east) south
      in
      let _, side =
        let f =
          joiner
            ~get:Option.some
            ~join:
              (straight_base
                 ~n_facets
                 ?height
                 ?fudge_factor
                 ?overlap_factor
                 ?min_width:min_straight_width )
        in
        Map.fold_right ~init:(None, Option.to_list south_corner) ~f body.sides.east
      and north_corner =
        draw_corner north (Option.map ~f:snd @@ Map.max_elt body.sides.east)
      in
      Util.prepend_opt north_corner side )
  in
  let south =
    let base i =
      if south_joins i
      then join_walls ?n_steps:body_join_steps ?fudge_factor ?overlap_factor
      else if i = pinky_idx && pinky_elbow
      then inward_elbow_base ~n_facets ~d:1.5 ?height ?n_steps
      else
        straight_base
          ~n_facets
          ?height
          ?fudge_factor
          ?overlap_factor
          ?min_width:min_straight_width
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
    and corner =
      Option.map2
        ~f:(join_walls ?n_steps:thumb_join_steps ~fudge_factor:0. ?overlap_factor)
    and link =
      Option.map2
        ~f:
          (snake_base
             ~n_facets
             ?height:thumb_height
             ?scale:snake_scale
             ?d:snake_d
             ?n_steps )
    in
    (* TODO: es and sw are pretty repetitive, can probably clean up, and also take
       lessons from using closest key into other places. *)
    let es =
      match corner thumb.sides.east e_s with
      | Some _ as es -> es
      | None         ->
        Map.closest_key thumb.keys `Less_than (Map.length thumb.keys)
        |> Option.bind ~f:(fun (_, k) -> k.Walls.Thumb.south)
        |> Option.map2 ~f:(bez_base ?n_steps ?height:thumb_height) thumb.sides.east
    and e_link =
      if Option.is_some thumb.sides.east
      then Option.map2 ~f:(bez_base ?n_steps ?height) (col `S 2) thumb.sides.east
      else link (col `S 2) e_s
    and sw =
      match corner w_s thumb.sides.west with
      | Some _ as es -> es
      | None         ->
        let next =
          Map.closest_key thumb.keys `Greater_or_equal_to 1
          |> Option.bind ~f:(fun (_, k) -> k.Walls.Thumb.south)
        in
        Option.map2
          ~f:
            (cubic_base
               ~n_facets
               ?n_steps
               ?height:thumb_height
               ?scale:thumb_cubic_scale
               ?d:thumb_cubic_d )
          next
          thumb.sides.west
    and nw = corner thumb.sides.west w_n
    and w_link =
      let f =
        if west_link_cubic
        then
          cubic_base
            ~n_facets
            ?height:thumb_height
            ?scale:thumb_cubic_scale
            ?d:thumb_cubic_d
            ?n_steps
            ~bow_out:false
        else
          straight_base
            ~n_facets
            ~height:index_height
            ?fudge_factor
            ?overlap_factor
            ?min_width:min_straight_width
      in
      Option.map2 ~f (Option.first_some w_n thumb.sides.west) (Map.find body.sides.west 0)
    in
    let east_swoop =
      Util.prepend_opt e_link
      @@
      if close_thumb
      then (
        let _, scads =
          let join = join_walls ?n_steps:thumb_join_steps ?fudge_factor ?overlap_factor
          and get d = d.Walls.Thumb.south in
          Map.fold_right ~init:(None, Option.to_list es) ~f:(joiner ~get ~join) thumb.keys
        in
        List.rev scads )
      else
        Option.map2 ~f:(bez_base ?height:thumb_height ?n_steps) e_s w_s |> Option.to_list
    in
    east_swoop, List.filter_opt [ sw; nw; w_link ]
  in
  (* TODO: Do unions of each of these separately, then clockwise union them.
     Test whether breakage leads to just part of the connection "disappearing" on
     CGAL failure, rather than the whole thing. (make it easier to pin point what
     is actually failing.) *)
  List.join [ west; north; east; south; east_swoop; west_swoop ] |> clockwise_union

let closed
    ?(join_west = true)
    ?(n_facets = 1)
    ?n_steps
    ?fudge_factor
    ?overlap_factor
    ?snake_d
    ?snake_scale
    ?snake_height
    ?snake_steps
    ?cubic_height
    ?cubic_scale
    ?cubic_d
    Walls.{ body; thumb }
  =
  let corner = Option.map2 ~f:(join_walls ?n_steps ~fudge_factor:0. ?overlap_factor) in
  let n_cols = Map.length body.cols
  and col side i =
    let%bind.Option c = Map.find body.cols i in
    match side with
    | `N -> c.north
    | `S -> c.south
  and prepend_corner w1 w2 = Util.prepend_opt (corner w1 w2) in
  let east, west =
    let f =
      joiner ~get:Option.some ~join:(join_walls ?n_steps ?fudge_factor ?overlap_factor)
    in
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
        ~join:(join_walls ?n_steps ?fudge_factor ?overlap_factor)
    in
    let _, north = Map.fold ~init:(None, []) ~f:(f `N) body.cols in
    let _, south = Map.fold_right ~init:(None, []) ~f:(f `S) body.cols in
    ( List.rev north
    , prepend_corner (Map.find body.sides.east 0) (col `S (n_cols - 1)) (List.rev south) )
  in
  let thumb =
    let _, south =
      let join = join_walls ?n_steps ?fudge_factor ?overlap_factor
      and get d = d.Walls.Thumb.south in
      Map.fold_right ~init:(None, []) ~f:(joiner ~get ~join) thumb.keys
    in
    let southwest =
      Option.bind ~f:(fun (_, k) -> k.Walls.Thumb.south) (Map.min_elt thumb.keys)
    and northwest =
      Option.bind ~f:(fun (_, k) -> k.Walls.Thumb.north) (Map.min_elt thumb.keys)
    and southeast =
      Option.bind ~f:(fun (_, k) -> k.Walls.Thumb.south) (Map.max_elt thumb.keys)
    and west_body = Option.map ~f:snd @@ Map.min_elt body.sides.west in
    let e_link =
      (Option.map2
         ~f:
           (snake_base
              ~n_facets
              ?height:snake_height
              ?scale:snake_scale
              ?d:snake_d
              ?n_steps:snake_steps ) )
        (col `S 2)
        southeast
    and w_link =
      let link =
        if join_west
        then corner
        else
          Option.map2
            ~f:
              (cubic_base
                 ~n_facets
                 ?height:cubic_height
                 ?scale:cubic_scale
                 ?d:cubic_d
                 ?n_steps
                 ~bow_out:false )
      in
      link (Option.first_some northwest thumb.sides.west) west_body
    in
    prepend_corner southwest thumb.sides.west south
    |> prepend_corner thumb.sides.west northwest
    |> Util.prepend_opt w_link
    |> List.rev
    |> prepend_corner thumb.sides.east southeast
    |> Util.prepend_opt e_link
  in
  List.join [ west; north; east; south; thumb ] |> clockwise_union

let to_scad t = t.scad
