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
  { scad : Scad.d3
  ; outline : Vec3.t list
  ; inline : Vec3.t list
  }
[@@deriving scad]

let centre (top, right, bot, left) = (left +. right) /. 2., (top +. bot) /. 2.

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
  { scad = Scad.union_3d scads; outline = List.rev out_pts; inline = List.rev in_pts }

let outline_2d t = List.map ~f:Vec3.to_vec2 t.outline
let inline_2d t = List.map ~f:Vec3.to_vec2 t.inline

let facet_points ?(rev = false) ?(n_facets = 1) ?(init = []) ~height bez =
  let step = height /. Float.of_int n_facets
  and start, continue, next =
    if rev then 1, ( >= ) n_facets, ( + ) 1 else n_facets, ( < ) 0, ( + ) (-1)
  in
  let rec loop ps i =
    if continue i then
      loop (Wall.Edge.point_at_z bez (Float.of_int i *. step) :: ps) (next i)
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
  let ((dx, dy, _) as dir1) = Wall.foot_direction w1 and dir2 = Wall.foot_direction w2 in
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
    ?(height = 10.)
    ?(scale = 1.1)
    ?(d = 2.)
    ?(n_steps = 10)
    ?(bow_out = true)
    (w1 : Wall.t)
    (w2 : Wall.t) =
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
    ?(height = 11.)
    ?(scale = 1.5)
    ?(d = 2.)
    ?(n_steps = 12)
    (w1 : Wall.t)
    (w2 : Wall.t) =
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
    (w2 : Wall.t) =
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
    (w2 : Wall.t) =
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
      if Float.(abs minor_diff > abs major_diff) then
        Float.(abs (min (abs major_diff -. fudge_factor) 0.)) /. 2.
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
    if
      Float.(
        (not (Sign.equal (sign_exn major_diff) (sign_exn major_ax)))
        && abs major_diff > abs minor_diff
        || intersect > 0.)
    then
      let rough_area =
        Vec3.(
          distance w1.foot.top_right w1.foot.top_left
          *. distance w1.foot.top_right w1.foot.bot_right)
      in
      Float.max (Float.abs major_diff) (intersect *. rough_area) *. overlap_factor
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
      if og then w1.edges.bot_right
      else if not outward then w1.edges.bot_right >> fudge dir1
      else w1.edges.bot_right >> fudge (Vec3.negate dir1)
    and bot_pt =
      if og then w1.foot.bot_right
      else if not outward then fudge dir1 w1.foot.bot_right
      else fudge (Vec3.negate dir1) w1.foot.bot_right
    in
    endpoints ~n_facets ~height w1.foot.top_right w1.edges.top_right bot_bez bot_pt
    |> List.map ~f:Vec3.(add (mul dir1 (overlap, overlap, 0.)))
  and dests og =
    let slide = fudge (Vec3.negate dir2) in
    let bot_bez =
      if og then w2.edges.bot_left
      else if outward then w2.edges.bot_left >> slide
      else w2.edges.bot_left >> fudge dir2
    and bot_pt =
      if og then w2.foot.bot_left
      else if not outward then fudge dir2 w2.foot.bot_left
      else fudge (Vec3.negate dir2) w2.foot.bot_left
    in
    endpoints ~n_facets ~height w2.foot.top_left w2.edges.top_left bot_bez bot_pt
    |> List.map ~f:Vec3.(add (mul dir2 (-0.05, -0.05, 0.)))
  in
  let extra_starts = starts false and extra_dests = dests false in
  { scad =
      Scad.union
        [ Util.prism_exn extra_starts extra_dests
        ; ( if outward then Util.prism_exn (starts true) extra_starts
          else Util.prism_exn extra_dests (dests true) )
        ]
  ; outline = [ w1.foot.top_right; w2.foot.top_left ]
  ; inline =
      List.map
        ~f:List.last_exn
        ( if outward then [ starts true; extra_starts; extra_dests ]
        else [ extra_starts; extra_dests; dests true ] )
  }

let join_walls
    ?(n_steps = `Flat 6)
    ?(fudge_factor = 3.)
    ?(overlap_factor = 1.2)
    (w1 : Wall.t)
    (w2 : Wall.t) =
  let ((dx, dy, _) as dir1) = Wall.foot_direction w1
  and dir2 = Wall.foot_direction w2
  and n_steps =
    let summit (w : Wall.t) =
      Points.fold ~f:(fun m p -> Float.max (Vec3.get_z p) m) ~init:Float.min_value w.start
    in
    Wall.Steps.to_int n_steps (Float.max (summit w1) (summit w2))
  in
  let major_diff, minor_diff =
    let x, y, _ =
      Vec3.(
        mean [ w1.foot.bot_right; w1.foot.top_right ]
        <-> mean [ w2.foot.bot_left; w2.foot.top_left ])
    in
    if Float.(abs dx > abs dy) then x, y else y, x
  in
  let outward =
    (* away from the centre of mass, or not? *)
    Float.(
      Vec3.distance w1.foot.top_right w2.foot.top_left
      > Vec3.distance w1.foot.top_right w2.foot.bot_left)
  and overhang =
    (* Obtuse angle between wall top difference and the foot difference indicates
       that the start wall is likely dodging the column below. If this is a corner,
       don't flag. *)
    let foot_diff = Vec3.(w1.foot.top_right <-> w2.foot.top_right)
    and top_diff = Vec3.(mul (w1.start.top_right <-> w2.start.top_left) (1., 1., 0.)) in
    let mag = Vec3.norm top_diff in
    if Float.(mag > 0.1 && Vec3.dot foot_diff top_diff < 0.) then Some mag else None
  in
  (* Move the start or destination points along the outer face of the wall to improve angle. *)
  let fudge start =
    if (not outward) && not start then
      let extra =
        if Float.(Vec3.(get_z w1.start.top_right > get_z w2.start.top_left)) then
          Float.(abs (max (fudge_factor -. major_diff) 0.))
        else 0.
      in
      Vec3.(add (mul dir2 (-.extra, -.extra, 0.)))
    else Fn.id
  and overlap =
    match overhang with
    | Some over -> over *. overlap_factor
    | None      ->
      let major_ax = if Float.(abs dx > abs dy) then dx else dy
      and intersect = Points.overlapping_bounds w1.foot w2.foot in
      if
        Float.(
          (not (Sign.equal (sign_exn major_diff) (sign_exn major_ax)))
          && abs major_diff > abs minor_diff
          || intersect > 0.)
      then
        let rough_area =
          Vec3.(
            distance w1.foot.top_right w1.foot.top_left
            *. distance w1.foot.top_right w1.foot.bot_right)
        in
        Float.max (Float.abs major_diff) (intersect *. rough_area) *. overlap_factor
      else 0.01
    (* If the walls are overlapping, move back the start positions to counter. *)
  in
  (* HACK: The way I am using the overhang flag here seemed to partially rescue one case,
     but I am not confident that it is a solid fix. *)
  let top_start, starts =
    let shove = Vec3.(add (mul dir1 (overlap, overlap, 0.))) in
    let top =
      w1.edges.top_right 0.
      |> fudge true
      |> (if Option.is_some overhang then Fn.id else shove)
      |> w1.edge_drawer.top
    in
    ( top
    , Bezier.curve_rev
        ~n_steps
        ~init:
          (Bezier.curve ~n_steps (w1.edge_drawer.bot (shove @@ w1.edges.bot_right 0.)))
        top )
  and top_dest, dests =
    let shove = Vec3.(add (mul dir2 (-.overlap, -.overlap, 0.))) in
    let top =
      w2.edges.top_left 0.
      |> fudge false
      |> (if Option.is_some overhang then Fn.id else shove)
      |> w2.edge_drawer.top
    in
    ( top
    , Bezier.curve_rev
        ~n_steps
        ~init:(Bezier.curve ~n_steps (w2.edge_drawer.bot (shove @@ w2.edges.bot_left 0.)))
        top )
  in
  let wedge =
    (* Fill in the volume between the "wedge" hulls that are formed by swinging the
     * key face and moving it out for clearance prior to drawing the walls. *)
    Util.prism_exn
      (List.map
         ~f:Vec3.(add (mul (Wall.start_direction w1) (overlap, overlap, 0.)))
         [ w1.start.top_right
         ; w1.start.bot_right
         ; w1.edges.bot_right 0.
         ; top_start 0.001
         ] )
      (List.map
         ~f:Vec3.(add (mul (Wall.start_direction w2) (-0.01, -0.01, 0.)))
         [ w2.start.top_left; w2.start.bot_left; w2.edges.bot_left 0.; top_dest 0.001 ] )
  in
  { scad = Scad.union [ Util.prism_exn starts dests; wedge ]
  ; outline = [ fudge true w1.foot.top_right; fudge false w2.foot.top_left ]
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

type config =
  | Straight of
      { n_facets : int option
      ; height : float option
      ; fudge_factor : float option
      ; overlap_factor : float option
      ; min_width : float option
      }
  | Bez of
      { n_facets : int option
      ; height : float option
      ; n_steps : int option
      }
  | Cubic of
      { n_facets : int option
      ; height : float option
      ; scale : float option
      ; d : float option
      ; n_steps : int option
      ; bow_out : bool option
      }
  | Snake of
      { n_facets : int option
      ; height : float option
      ; scale : float option
      ; d : float option
      ; n_steps : int option
      }
  | FullJoin of
      { n_steps : Wall.Steps.t option
      ; fudge_factor : float option
      ; overlap_factor : float option
      }
  | InwardElbow of
      { n_facets : int option
      ; height : float option
      ; n_steps : int option
      ; d : float option
      }

let straight ?n_facets ?height ?fudge_factor ?overlap_factor ?min_width () =
  Straight { n_facets; height; fudge_factor; overlap_factor; min_width }

let bez ?n_facets ?height ?n_steps () = Bez { n_facets; height; n_steps }

let cubic ?n_facets ?height ?scale ?d ?n_steps ?bow_out () =
  Cubic { n_facets; height; scale; d; n_steps; bow_out }

let snake ?n_facets ?height ?scale ?d ?n_steps () =
  Snake { n_facets; height; scale; d; n_steps }

let full_join ?n_steps ?fudge_factor ?overlap_factor () =
  FullJoin { n_steps; fudge_factor; overlap_factor }

let elbow ?n_facets ?height ?n_steps ?d () = InwardElbow { n_facets; height; n_steps; d }

let connect = function
  | Straight { n_facets; height; fudge_factor; overlap_factor; min_width } ->
    straight_base ?n_facets ?height ?fudge_factor ?overlap_factor ?min_width
  | Bez { n_facets; height; n_steps } -> bez_base ?n_facets ?height ?n_steps
  | Cubic { n_facets; height; scale; d; n_steps; bow_out } ->
    cubic_base ?n_facets ?height ?scale ?d ?n_steps ?bow_out
  | Snake { n_facets; height; scale; d; n_steps } ->
    snake_base ?n_facets ?height ?scale ?d ?n_steps
  | FullJoin { n_steps; fudge_factor; overlap_factor } ->
    join_walls ?n_steps ?fudge_factor ?overlap_factor
  | InwardElbow { n_facets; height; n_steps; d } ->
    inward_elbow_base ?n_facets ?height ?n_steps ?d

let manual_joiner ~get ~join ~key ~data (i, last, scads) =
  let next = get data in
  let scads' =
    match Option.map2 ~f:(connect @@ join i) last next with
    | Some j -> j :: scads
    | None   -> scads
  in
  match next, last with
  | Some _, _    -> key, next, scads'
  | None, Some _ -> i, last, scads'
  | _            -> i, last, scads'

let manual
    ?(west = fun _ -> bez ())
    ?(north =
      function
      | i when i < 2 -> full_join ()
      | i when i = 4 -> cubic ()
      | _ -> straight ())
    ?(south = fun _ -> straight ())
    ?(east = fun _ -> bez ())
    ?(east_link = snake ())
    ?(thumb_east = fun _ -> full_join ())
    ?(thumb_south = fun _ -> full_join ())
    ?(thumb_west = fun _ -> full_join ())
    ?(thumb_north = fun _ -> straight ())
    ?(west_link = cubic ~bow_out:false ())
    Walls.{ body; thumb } =
  let southeast = Option.map ~f:snd (Map.max_elt body.south) in
  let west =
    let northwest = Map.find body.north 0
    and last_idx, last, side =
      let f = manual_joiner ~get:Option.some ~join:west in
      Map.fold ~init:(0, None, []) ~f body.west
    in
    List.rev
    @@ Util.prepend_opt (Option.map2 ~f:(connect (west last_idx)) last northwest) side
  in
  let north =
    let last_idx, last, side =
      let f = manual_joiner ~get:Option.some ~join:north in
      Map.fold ~init:(0, None, []) ~f body.north
    and next =
      (* if there is nothing in the east, connect to the southern corner *)
      Option.first_some (Option.map ~f:snd @@ Map.max_elt body.east) southeast
    in
    List.rev
    @@ Util.prepend_opt (Option.map2 ~f:(connect (north last_idx)) last next) side
  in
  let east =
    let south_corner =
      let last_idx, last =
        match Map.min_elt body.east with
        | Some (i, w) -> i, Some w
        | None        -> 0, None
      in
      Option.map2 ~f:(connect (east last_idx)) last southeast
    and _, _, side =
      let f = manual_joiner ~get:Option.some ~join:east in
      Map.fold_right ~init:(0, None, []) ~f body.east
    in
    Util.prepend_opt south_corner side |> List.rev
  and last_south, south =
    let _, last, side =
      let f = manual_joiner ~get:Option.some ~join:south in
      Map.fold_right ~init:(0, None, []) ~f body.south
    in
    last, List.rev side
  in
  let thumb_swoop =
    let last_idx, last_thumb_south, swoop =
      let southeast = Option.map ~f:snd (Map.max_elt thumb.south)
      and first =
        Option.first_some
          (Map.find thumb.east 0)
          (Option.map ~f:snd (Map.max_elt thumb.south))
      in
      let e_link = Option.map2 ~f:(connect east_link) last_south first in
      let last_idx, last_east, east =
        let i, last, side =
          let f = manual_joiner ~get:Option.some ~join:thumb_east in
          Map.fold ~init:(0, None, []) ~f thumb.east
        in
        i, last, List.rev (Util.prepend_opt e_link side)
      in
      let se = Option.map2 ~f:(connect (thumb_east last_idx)) last_east southeast in
      let f = manual_joiner ~get:Option.some ~join:thumb_south in
      Map.fold_right ~init:(0, None, Util.prepend_opt se east) ~f thumb.south
    in
    let last_thumb_west, swoop =
      let sw =
        let first_west = Option.map ~f:snd (Map.max_elt thumb.west) in
        Option.map2 ~f:(connect (thumb_south last_idx)) last_thumb_south first_west
      and last_west, west =
        let northwest = Map.find thumb.north 0
        and last_idx, last, side =
          let f = manual_joiner ~get:Option.some ~join:thumb_west in
          Map.fold_right ~init:(0, None, swoop) ~f thumb.west
        in
        ( last
        , List.rev
          @@ Util.prepend_opt
               (Option.map2 ~f:(connect (thumb_west last_idx)) last northwest)
               side )
      in
      last_west, Util.prepend_opt sw west
    in
    let _, last, swoop =
      let f = manual_joiner ~get:Option.some ~join:thumb_north in
      Map.fold ~init:(0, None, swoop) ~f thumb.north
    in
    Util.prepend_opt
      (Option.map2
         ~f:(connect west_link)
         (Option.first_some last (Option.first_some last_thumb_west last_thumb_south))
         (Option.map ~f:snd @@ Map.min_elt body.west) )
      swoop
    |> List.rev
  in
  (* unions separately, followed by final union so failures in CGAL can be narrowed
     down more easily (a section disappears, rather than the whole thing) *)
  List.map ~f:clockwise_union [ west; north; east; south; thumb_swoop ] |> clockwise_union

(* TODO: re-write skeleton to use manual (as with closed). *)
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
    ?cubic_d
    ?cubic_scale
    ?thumb_height
    ?(east_link = snake ())
    ?(west_link = cubic ~bow_out:false ())
    ?(north_joins = fun i -> i < 2)
    ?(south_joins = fun _ -> false)
    ?(pinky_idx = 4)
    ?(pinky_elbow = true)
    ?(close_thumb = false)
    Walls.{ body; thumb } =
  (* TODO:
     - bez_base is not great on the northeast corner when there is actually a wall
       on that side that needs to be connected, because it was designed for cornering
       like it is used on the west side.
     - should still do some clean-up on this, especially now that I ripped through here
       adjusting to the new type t. *)
  let n_cols = Map.length body.north in
  let west =
    let corner =
      Option.map2
        ~f:(bez_base ~height:index_height ?n_steps)
        (Option.map ~f:snd @@ Map.max_elt body.west)
        (Map.find body.north 0)
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
      Map.fold ~init:(None, []) ~f body.west
    in
    List.rev @@ Util.prepend_opt corner side
  in
  let north =
    List.init
      ~f:(fun i ->
        Option.map2
          ~f:
            ( if north_joins i then
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
          (Map.find body.north i)
          (Map.find body.north (i + 1)) )
      (n_cols - 1)
    |> List.filter_opt
  in
  let east =
    let north = Map.find body.north (n_cols - 1)
    and south = Map.find body.south (n_cols - 1) in
    if Map.is_empty body.east then
      Option.map2
        ~f:(cubic_base ~n_facets ?height ?d:cubic_d ?scale:cubic_scale ?n_steps)
        north
        south
      |> Option.to_list
    else
      let draw_corner = Option.map2 ~f:(bez_base ?height ?n_steps) in
      let south_corner = draw_corner (Option.map ~f:snd @@ Map.min_elt body.east) south in
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
        Map.fold_right ~init:(None, Option.to_list south_corner) ~f body.east
      and north_corner = draw_corner north (Option.map ~f:snd @@ Map.max_elt body.east) in
      Util.prepend_opt north_corner side
  in
  let south =
    let base i =
      if south_joins i then
        join_walls ?n_steps:body_join_steps ?fudge_factor ?overlap_factor
      else if i = pinky_idx && pinky_elbow then
        inward_elbow_base ~n_facets ~d:1.5 ?height ?n_steps
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
        Option.map2
          ~f:(base idx)
          (Map.find body.south idx)
          (Map.find body.south (idx - 1)) )
      (n_cols - 1)
    |> List.filter_opt
  in
  let east_swoop, west_swoop =
    let w_n = Option.map ~f:snd @@ Map.min_elt thumb.north
    and w_s = Option.map ~f:snd @@ Map.min_elt thumb.south
    (* let Walls.Thumb.{ north = w_n; south = w_s } = Map.find_exn thumb.keys 0 *)
    and e_s = Option.map ~f:snd @@ Map.max_elt thumb.south
    (* and _, Walls.Thumb.{ south = e_s; _ } = Map.max_elt_exn thumb.keys *)
    and last_west = Option.map ~f:snd @@ Map.min_elt thumb.west
    and corner =
      Option.map2
        ~f:(join_walls ?n_steps:thumb_join_steps ~fudge_factor:0. ?overlap_factor)
    and link = Option.map2 ~f:(connect east_link) in
    (* TODO: es and sw are pretty repetitive, can probably clean up, and also take
       lessons from using closest key into other places. *)
    let es =
      let last_east = Option.map ~f:snd @@ Map.max_elt thumb.east in
      match corner last_east e_s with
      | Some _ as es -> es
      | None         ->
        Map.closest_key thumb.south `Less_than (Map.length thumb.south)
        |> Option.map ~f:snd
        |> Option.map2 ~f:(bez_base ?n_steps ?height:thumb_height) last_east
    and e_link =
      let first_east = Option.map ~f:snd @@ Map.min_elt thumb.east in
      if Option.is_some first_east then
        Option.map2 ~f:(bez_base ?n_steps ?height) (Map.find body.south 2) first_east
      else link (Map.find body.south 2) e_s
    and sw =
      let first_west = Option.map ~f:snd @@ Map.max_elt thumb.west in
      match corner w_s first_west with
      | Some _ as sw -> sw
      | None         ->
        let next =
          Option.map ~f:snd @@ Map.closest_key thumb.south `Greater_or_equal_to 1
        in
        Option.map2
          ~f:
            (straight_base
               ~n_facets
               ?height:thumb_height
               ?fudge_factor
               ?overlap_factor
               ?min_width:min_straight_width )
          next
          first_west
    and nw = corner last_west w_n
    and w_link =
      let f = connect west_link in
      Option.map2
        ~f
        (Option.first_some w_n last_west)
        (Option.map ~f:snd @@ Map.min_elt body.west)
    in
    let east_swoop =
      Util.prepend_opt e_link
      @@
      if close_thumb then
        let _, scads =
          let join = join_walls ?n_steps:thumb_join_steps ?fudge_factor ?overlap_factor in
          Map.fold_right
            ~init:(None, Option.to_list es)
            ~f:(joiner ~get:Option.some ~join)
            thumb.south
        in
        List.rev scads
      else
        Option.map2 ~f:(bez_base ?height:thumb_height ?n_steps) e_s w_s |> Option.to_list
    in
    east_swoop, List.filter_opt [ sw; nw; w_link ]
  in
  (* unions separately, followed by final union so failures in CGAL can be narrowed
     down more easily (a section disappears, rather than the whole thing) *)
  List.map ~f:clockwise_union [ west; north; east; south; east_swoop; west_swoop ]
  |> clockwise_union

let closed
    ?body_steps
    ?thumb_steps
    ?fudge_factor
    ?overlap_factor
    ?(west_link = full_join ~fudge_factor:0. ())
    ?(east_link = snake ())
    (Walls.{ body; _ } as walls) =
  let join = full_join ?fudge_factor ?overlap_factor
  and corner = full_join ~fudge_factor:0. ?overlap_factor in
  let side ?n_steps last_idx i =
    if i = last_idx then corner ?n_steps () else join ?n_steps ()
  and max_key m = fst (Map.max_elt_exn m) in
  let north =
    let last_idx = max_key body.north in
    if Map.length walls.body.east = 0 then fun i ->
      if i = last_idx then cubic ~height:10. () else join ?n_steps:body_steps ()
    else side ?n_steps:body_steps last_idx
  in
  manual
    ~west:(side ?n_steps:body_steps (max_key body.west))
    ~north
    ~east:(side ?n_steps:body_steps 0)
    ~south:(fun _ -> join ?n_steps:body_steps ())
    ~east_link
    ~thumb_east:(fun _ -> corner ?n_steps:thumb_steps ())
    ~thumb_south:(side ?n_steps:thumb_steps 0)
    ~thumb_west:(fun _ -> corner ?n_steps:thumb_steps ())
    ~thumb_north:(fun _ -> join ?n_steps:thumb_steps ())
    ~west_link
    walls

let to_scad t = t.scad
