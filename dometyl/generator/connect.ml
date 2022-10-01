open! Scad_ml
open! Syntax

type t =
  { scad : Scad.d3
  ; outline : Path3.t
  ; inline : Path3.t
  }
[@@deriving scad]

let clockwise_union ts =
  let collect ~init line = List.fold_left (fun ps p -> p :: ps) init line in
  let f (scads, out_pts, in_pts) { scad; outline; inline } =
    scad :: scads, collect ~init:out_pts outline, collect ~init:in_pts inline
  in
  let scads, out_pts, in_pts = List.fold_left f ([], [], []) ts in
  { scad = Scad.union3 scads; outline = List.rev out_pts; inline = List.rev in_pts }

let outline_2d t = Path3.to_path2 t.outline
let inline_2d t = Path3.to_path2 t.inline

(* since connections are always made in the clockwise direction, a ccw sign can
   here can be interpreted as w2 being "outward" (away from plate centre)
   relative to w1 *)
let outward_sign (w1 : Wall.t) (w2 : Wall.t) =
  V3.clockwise_sign w1.foot.top_left w1.foot.top_right w2.foot.top_left

(* Apply the roundover corner to the two topmost points of the connector drawn
   by bot_edge and top_edge. Edges must have the same length. *)
let rounder ?fn ~corner bot_edge top_edge =
  let corners = Some corner :: List.init (List.length bot_edge - 1) (Fun.const None) in
  List.(rev_append (combine bot_edge corners) (combine top_edge corners))
  |> Path3.Round.mix
  |> Path3.roundover ?fn ~overrun:`Fix

let fillet ~d ~h rows =
  let rel_dists, total_dist =
    let f (dists, sum, last) row =
      let p = V3.(mean row *@ v 1. 1. 0.) in
      let sum = sum +. V3.distance p last in
      sum :: dists, sum, p
    and start = V3.(mean (List.hd rows) *@ v 1. 1. 0.) in
    let dists, sum, _ = List.fold_left f ([ 0. ], 0., start) (List.tl rows) in
    List.rev_map (fun d -> d /. sum) dists, sum
  in
  let ez =
    let d =
      match d with
      | `Rel d -> d
      | `Abs d -> d /. total_dist
    in
    Easing.make (v2 d 1.) (v2 d 1.)
  in
  let f =
    let h =
      match h with
      | `Rel h -> h
      | `Abs h -> h /. (Path3.bbox (List.hd rows)).max.z
    in
    fun u row ->
      let u = if u > 0.5 then 1. -. u else u in
      Path3.scale (v3 1. 1. (1. -. (ez u *. h))) row
  in
  List.map2 f rel_dists rows

let spline_base
    ?(height = 11.)
    ?(d = 1.)
    ?(size = 8.)
    ?(fn = 64)
    ?(min_step_dist = 0.02)
    ?(fillet_d = `Rel 0.2)
    ?(fillet_h = `Rel 0.3)
    ?(corner = Path3.Round.bez (`Joint 1.))
    ?(corner_fn = 6)
    ?(max_edge_res = 0.5)
    ?(end_shrink = 0.025)
    ?(tight_threshold = 6.)
    ?(tight_d = 0.05)
    (w1 : Wall.t)
    (w2 : Wall.t)
  =
  let dir1 = Wall.foot_direction w1
  and dir2 = Wall.foot_direction w2 in
  let end_edges ?(shrink = 0.) ?frac left =
    let edger (w : Wall.t) corner =
      let edge = w.drawer corner in
      let u =
        Path3.continuous_closest_point
          ~n_steps:45
          (Path3.to_continuous edge)
          (V3.add (Util.last edge) (v3 0. 0. height))
      in
      snd @@ Path3.split ~distance:(`Rel u) edge
    in
    let w, dir =
      if left then w1, V3.neg @@ Wall.foot_direction w1 else w2, Wall.foot_direction w2
    in
    let b_loc, t_loc =
      let s = shrink /. 2. in
      match left, frac with
      | true, None -> `XY (0.99, s), `XY (0.99, 1. -. s) (* fudging for union *)
      | false, None -> `XY (0.01, s), `XY (0.01, 1. -. s)
      | true, Some frac -> `XY (1. -. frac, s), `XY (1. -. frac, 1. -. s)
      | false, Some frac -> `XY (frac, s), `XY (frac, 1. -. s)
    in
    let bot = edger w b_loc
    and top = edger w t_loc in
    let plane = Plane.of_normal ~point:(Util.last bot) dir in
    let dedup =
      let proj = V3.project plane in
      let eq a b = V2.approx ~eps:max_edge_res (proj a) (proj b) in
      Path3.deduplicate_consecutive ~keep:`FirstAndEnds ~eq
    in
    plane, Path3.prune_collinear @@ dedup bot, Path3.prune_collinear @@ dedup top
  in
  let plane_l, bot_l, top_l = end_edges true
  and plane_r, bot_r, top_r = end_edges false in
  let edge_len =
    let l = Int.max (List.length bot_l) (List.length top_l)
    and r = Int.max (List.length bot_r) (List.length top_r) in
    Int.max l r
  in
  let subdiv = Path3.subdivide ~freq:(`N (edge_len, `ByLen)) in
  let bot_l, top_l, bot_r, top_r =
    subdiv bot_l, subdiv top_l, subdiv bot_r, subdiv top_r
  in
  let planer plane bot top =
    let f (d, acc) p =
      let p' = Plane.lift plane (Plane.project plane p) in
      Float.max (Plane.distance_to_point plane p) d, p' :: acc
    in
    let d, top' = List.fold_left f (0., []) top in
    let d, bot' = List.fold_left f (d, []) bot in
    (* NOTE: 0.3 feels like a pretty large fudge, but I have seen issues with
        lower, how often do those cases actually come up, I have had it working
        with 0.05 before running into a situation that seemed to call for 0.3.
        Another param to expose? *)
    let f = V3.(translate (Plane.normal plane *$ (d +. 0.3))) in
    List.rev_map f bot', List.rev_map f top'
  in
  let bot_l', top_l' = planer plane_l bot_l top_l
  and bot_r', top_r' = planer plane_r bot_r top_r in
  let round = rounder ~fn:corner_fn ~corner in
  let a' = round bot_l' top_l'
  and b' = round bot_r' top_r' in
  let row_len = Int.max (List.length a') (List.length b') in
  let a' = Path3.subdivide ~closed:true ~freq:(`N (row_len, `ByLen)) a'
  and b' = Path3.subdivide ~closed:true ~freq:(`N (row_len, `ByLen)) b' in
  let base_points = function
    | inner :: pts -> inner, Util.last pts
    | [] -> failwith "unreachable"
  in
  let p0 = V3.(mean a' *@ v3 1. 1. 0.)
  and p3 = V3.(mean b' *@ v3 1. 1. 0.) in
  let a' = Path3.translate (V3.neg p0) a'
  and b' = Path3.translate (V3.neg p3) b' in
  let align_q = Quaternion.align dir2 dir1 in
  let b' = Path3.quaternion align_q b' in
  let ang = V3.angle (V3.neg dir1) V3.(p3 -@ p0) *. outward_sign w1 w2 in
  let dist = V2.(distance (v p0.x p0.y) (v p3.x p3.y)) in
  let ang_ratio = dist /. Float.abs ang /. Float.pi in
  let step = 1. /. Float.of_int fn in
  let transition i = List.map2 (fun a b -> V3.lerp a b (Float.of_int i *. step)) a' b' in
  let out =
    if ang_ratio < tight_threshold && V3.angle dir1 dir2 < 3.
    then tight_d
    else Float.min d (dist /. 4.)
  in
  let transforms =
    let path =
      let p1 = V3.(p0 +@ (dir1 *$ -.out))
      and p2 = V3.(p3 +@ (dir2 *$ out)) in
      Bezier3.curve ~fn @@ Bezier3.of_path ~size:(`Flat (`Abs size)) [ p0; p1; p2; p3 ]
    in
    Util.prune_transforms ~min_dist:min_step_dist ~shape:transition
    @@ Path3.to_transforms ~mode:`NoAlign path
  in
  let in_start', out_start' = base_points a' in
  let scad =
    let rows =
      List.map (fun (i, m) -> Path3.affine m (transition i)) transforms
      |> fillet ~d:fillet_d ~h:fillet_h
    in
    let slices, last_row =
      (* while grabbing last row, make a slice count list (drop one to match
            number of transitions) *)
      let slices, r = List.fold_left (fun (zs, _) r -> 0 :: zs, r) ([ 5 ], []) rows in
      `Mix (5 :: List.tl slices), r
    and round_edges (_, bot, top) =
      let bot = Path3.prune_collinear bot
      and top = Path3.prune_collinear top in
      let n = Int.(max edge_len (max (List.length bot) (List.length top))) in
      let subdiv = Path3.subdivide ~freq:(`N (n, `ByLen)) in
      round (subdiv bot) (subdiv top)
    in
    let start = round_edges @@ end_edges ~shrink:end_shrink ~frac:0.01 true
    and finish =
      let d =
        let true_b' = Path3.quaternion (Quaternion.conj align_q) b' in
        let end_plane = Path3.to_plane (Path3.translate p3 true_b') in
        let f (mn, mx) p =
          let d = Plane.distance_to_point end_plane p in
          Float.min mn d, Float.max mx d
        in
        let below, above = List.fold_left f (Float.max_float, Float.min_float) last_row in
        (* compute how angled the last row is relative to the end_plane, and
            use that slope to scale how much the wall profile should be slid
            away to avoid self intersection during the linear skin transition *)
        let slope =
          let bot, top = base_points last_row in
          (above -. below) /. V3.distance bot top
        in
        above -. (below *. slope)
      in
      let rel = d /. V3.distance w2.foot.top_left w2.foot.top_right in
      round_edges @@ end_edges ~shrink:end_shrink ~frac:(Float.min 0.99 rel) false
    in
    Mesh.to_scad @@ Mesh.skin ~slices (List.append (start :: rows) [ finish ])
  and inline = List.map (fun (_, m) -> V3.affine m in_start') transforms
  and outline = List.map (fun (_, m) -> V3.affine m out_start') transforms in
  { scad; inline; outline }

let join_walls
    ?(max_angle = 1.4)
    ?(slices = 12)
    ?(gap_fill = true)
    (w1 : Wall.t)
    (w2 : Wall.t)
  =
  let outward = outward_sign w1 w2 > 0.
  and edges drawer frac =
    let f = Path3.deduplicate_consecutive ~keep:`FirstAndEnds ~eq:(V3.approx ~eps:1e-1) in
    let bot = f (drawer (`B frac))
    and top = f (drawer (`T frac)) in
    bot, top
  and profs l1 r1 l2 r2 =
    let subdiv =
      let n =
        let f n (bot, top) = Int.max n List.(Int.max (length bot) (length top)) in
        List.fold_left f 0 [ l1; r1; l2; r2 ]
      in
      Path3.subdivide ~closed:false ~freq:(`N (n, `ByLen))
    in
    let prof (bot, top) = List.append (subdiv top) (List.rev @@ subdiv bot) in
    prof l1, prof r1, prof l2, prof r2
  and sharpest (l1, r1, l2, r2) =
    let f (mx, ps) p1 p2 p3 p4 =
      let ang = Float.pi -. V3.(angle_points p1 p2 p3) in
      if ang > mx then ang, (p1, p2, p3, p4) else mx, ps
    in
    let open List in
    Util.fold_left4 f (0., (hd l1, hd r1, hd l2, hd r2)) (tl l1) (tl r1) (tl l2) (tl r2)
  in
  let sharp, (far_ap, ap, bp, far_bp) =
    let a = edges w1.bounds_drawer 1.
    and b = edges w2.bounds_drawer 0. in
    let far_a = edges w1.bounds_drawer 0.
    and far_b = edges w2.bounds_drawer 1. in
    sharpest (profs far_a a b far_b)
  in
  let a_frac', a_frac, b_frac, b_frac' =
    let dir_angle = V3.angle (Wall.start_direction w1) (Wall.start_direction w2) in
    if sharp > max_angle && not (Math.approx dir_angle (Float.pi /. 2.))
    then (
      (* use law of sines to compute shift required along the inner wall to
           bring the sharpest edge angle down to the max_angle *)
      let alpha = Float.pi -. max_angle
      and side_a = if outward then V3.distance far_ap bp else V3.distance ap far_bp
      and beta = V3.angle_points bp (if outward then far_ap else far_bp) ap in
      let gamma = Float.pi -. alpha -. beta in
      let full_c = if outward then V3.distance far_ap ap else V3.distance far_bp bp
      and shrunk_c = side_a *. Float.sin gamma /. Float.sin alpha in
      let frac = Float.min (shrunk_c /. full_c) 0.99 in
      if outward
      then frac -. 0.02, frac, 0.005, 0.03
      else 0.97, 0.995, 1. -. frac, 1. -. frac +. 0.02 )
    else 0.97, 0.99, 0.01, 0.03
  in
  let ((a_bot, a_top) as a) = edges w1.bounds_drawer a_frac
  and ((_, a'_top) as a') = edges w1.drawer a_frac'
  and ((_, b'_top) as b') = edges w2.drawer b_frac'
  and ((b_bot, b_top) as b) = edges w2.bounds_drawer b_frac in
  let tri =
    if gap_fill && sharp > max_angle
    then (
      let bot, top, bot', top', far_top' =
        let a_bot, a_top = edges w1.bounds_drawer (a_frac -. 0.001)
        and b_bot, b_top = edges w2.bounds_drawer (b_frac +. 0.001) in
        if outward
        then List.(hd b_bot, hd b_top, hd a_bot, hd a_top, hd a'_top)
        else List.(hd a_bot, hd a_top, hd b_bot, hd b_top, hd b'_top)
      in
      let w = if outward then w2 else w1 in
      let face =
        (* get key face perpendicular to the outer wall face *)
        match outward, w.side with
        | true, `North | false, `South -> w.key.faces.west
        | true, `East | false, `West -> w.key.faces.north
        | true, `South | false, `North -> w.key.faces.east
        | true, `West | false, `East -> w.key.faces.south
      in
      let plane = Path3.to_plane (Points.to_ccw_path face.bounds) in
      (* find the best points along the perpendicular edges to use as the third
            point of the gap filling triangle *)
      match Plane.line_intersection plane V3.{ a = far_top'; b = top' } with
      | `Point (pt, _) ->
        ( try
            let side l r =
              let f = Path3.to_continuous [ l; r ] in
              let u = Path3.continuous_closest_point ~n_steps:90 f pt
              and normal = V3.(normalize @@ (face.bounds.centre -@ w.key.origin)) in
              V3.(f u -@ (normal *$ 0.1))
              (* fudge in to avoid coincident face *)
            in
            let side_bot = side face.bounds.bot_left face.bounds.bot_right
            and side_top = side face.bounds.top_left face.bounds.top_right in
            let top_row = [ top; side_top; top' ]
            and bot_row = [ bot; side_bot; bot' ] in
            Some (Mesh.to_scad @@ Mesh.hull (List.rev_append bot_row top_row))
          with
        | Failure _ ->
          print_endline "NOTE: Full wall join gap filling failed.";
          None )
      | _ -> None (* inner edge line does not hit outer perpendicular face *) )
    else None (* no shift perfomed, so no gap to fill *)
  in
  let a', a, b, b' = profs a' a b b' in
  let scad =
    let s = Mesh.to_scad @@ Mesh.skin ~slices:(`Mix [ 2; slices; 2 ]) [ a'; a; b; b' ] in
    Util.value_map_opt ~default:s (Scad.add s) tri
  in
  Util.{ scad; outline = [ last a_top; last b_top ]; inline = [ last a_bot; last b_bot ] }

type config =
  | FullJoin of
      { slices : int option
      ; max_angle : float option
      ; gap_fill : bool option
      }
  | Spline of
      { height : float option
      ; d : float option
      ; size : float option
      ; fn : int option
      ; min_step_dist : float option
      ; fillet_d : [ `Abs of float | `Rel of float ] option
      ; fillet_h : [ `Abs of float | `Rel of float ] option
      ; corner : Path3.Round.corner option
      ; corner_fn : int option
      ; max_edge_res : float option
      ; end_shrink : float option
      ; tight_threshold : float option
      ; tight_d : float option
      }

let full_join ?slices ?max_angle ?gap_fill () = FullJoin { slices; max_angle; gap_fill }

let spline
    ?height
    ?d
    ?size
    ?fn
    ?min_step_dist
    ?fillet_d
    ?fillet_h
    ?corner
    ?corner_fn
    ?max_edge_res
    ?end_shrink
    ?tight_threshold
    ?tight_d
    ()
  =
  Spline
    { height
    ; d
    ; size
    ; fn
    ; min_step_dist
    ; fillet_d
    ; fillet_h
    ; corner
    ; corner_fn
    ; max_edge_res
    ; end_shrink
    ; tight_threshold
    ; tight_d
    }

let connect = function
  | Spline
      { height
      ; d
      ; size
      ; fn
      ; min_step_dist
      ; fillet_d
      ; fillet_h
      ; corner
      ; corner_fn
      ; max_edge_res
      ; end_shrink
      ; tight_threshold
      ; tight_d
      } ->
    spline_base
      ?height
      ?d
      ?size
      ?fn
      ?min_step_dist
      ?fillet_d
      ?fillet_h
      ?corner
      ?corner_fn
      ?max_edge_res
      ?end_shrink
      ?tight_threshold
      ?tight_d
  | FullJoin { slices; max_angle; gap_fill } -> join_walls ?slices ?max_angle ?gap_fill

let manual_joiner ~join key next (i, last, scads) =
  let scads' =
    match Option.map (fun l -> connect (join i) l next) last with
    | Some j -> j :: scads
    | None -> scads
  in
  key, Some next, scads'

let manual
    ?(west = fun _ -> spline ())
    ?(north =
      function
      | i when i < 2 -> full_join ()
      | i when i = 4 -> spline ()
      | _ -> spline ())
    ?(south = fun _ -> spline ())
    ?(east = fun _ -> spline ())
    ?(east_link = spline ())
    ?(thumb_east = fun _ -> spline ())
    ?(thumb_south = fun _ -> spline ())
    ?(thumb_west = fun _ -> spline ())
    ?(thumb_north = fun _ -> spline ())
    ?(west_link = spline ())
    Walls.{ body; thumb }
  =
  let southeast = Option.map snd (IMap.max_binding_opt body.south) in
  let fold ?(rev = false) ?(init = []) ~join walls =
    let fold = if rev then IMap.fold_right else IMap.fold in
    fold (manual_joiner ~join) walls (0, None, init)
  in
  let west =
    let northwest =
      let+ _, nw = IMap.min_binding_opt body.north in
      nw
    and last_idx, last, side = fold ~join:west body.west in
    List.rev
    @@ Util.prepend_opt (Util.map2_opt (connect (west last_idx)) last northwest) side
  in
  let north =
    let last_idx, last, side = fold ~join:north body.north
    and next =
      (* if there is nothing in the east, connect to the southern corner *)
      Util.first_some (Option.map snd @@ IMap.max_binding_opt body.east) southeast
    in
    List.rev @@ Util.prepend_opt (Util.map2_opt (connect (north last_idx)) last next) side
  in
  let east =
    let south_corner =
      let last_idx, last =
        match IMap.min_binding_opt body.east with
        | Some (i, w) -> i, Some w
        | None -> 0, None
      in
      Util.map2_opt (connect (east last_idx)) last southeast
    and _, _, side = fold ~rev:true ~join:east body.east in
    Util.prepend_opt south_corner side |> List.rev
  and last_south, south =
    let _, last, side = fold ~rev:true ~join:south body.south in
    last, List.rev side
  in
  let thumb_swoop =
    let last_idx, last_thumb_south, swoop =
      let southeast = Option.map snd (IMap.max_binding_opt thumb.south) in
      let first =
        Util.first_some (Option.map snd (IMap.min_binding_opt thumb.east)) southeast
      in
      let e_link = Util.map2_opt (connect east_link) last_south first in
      let last_idx, last_east, east =
        fold ~join:thumb_east ~init:(Option.to_list e_link) thumb.east
      in
      let se = Util.map2_opt (connect (thumb_east last_idx)) last_east southeast in
      fold ~rev:true ~join:thumb_south ~init:(Util.prepend_opt se east) thumb.south
    in
    let last_thumb_west, swoop =
      let northwest = Option.map snd (IMap.min_binding_opt thumb.north) in
      let sw =
        (* if there is nothing in the west, connect to the first northern wall *)
        let next =
          Util.first_some (Option.map snd (IMap.max_binding_opt thumb.west)) northwest
        in
        Util.map2_opt (connect (thumb_south last_idx)) last_thumb_south next
      in
      let last_idx, last, side =
        fold ~rev:true ~join:thumb_west ~init:(Util.prepend_opt sw swoop) thumb.west
      in
      ( last
      , Util.prepend_opt
          (Util.map2_opt (connect (thumb_west last_idx)) last northwest)
          side )
    in
    let _, last, swoop = fold ~join:thumb_north ~init:swoop thumb.north in
    Util.prepend_opt
      (Util.map2_opt
         (connect west_link)
         (Util.first_some last (Util.first_some last_thumb_west last_thumb_south))
         (Option.map snd @@ IMap.min_binding_opt body.west) )
      swoop
    |> List.rev
  in
  (* unions separately, followed by final union so failures in CGAL can be narrowed
     down more easily (a section disappears, rather than the whole thing) *)
  List.map clockwise_union [ west; north; east; south; thumb_swoop ] |> clockwise_union

let skeleton
    ?height
    ?(index_height = 13.)
    ?thumb_height
    ?spline_d
    ?spline_size
    ?fn
    ?min_step_dist
    ?fillet_d
    ?fillet_h
    ?corner
    ?corner_fn
    ?max_edge_res
    ?end_shrink
    ?tight_threshold
    ?tight_spline_d
    ?join_slices
    ?max_join_angle
    ?gap_fill
    ?east_link
    ?west_link
    ?(north_joins = fun i -> i < 2)
    ?(south_joins = fun _ -> false)
    ?(close_thumb = false)
    (Walls.{ body; _ } as walls)
  =
  let spline =
    spline
      ?d:spline_d
      ?fn
      ?size:spline_size
      ?min_step_dist
      ?fillet_d
      ?fillet_h
      ?corner
      ?corner_fn
      ?max_edge_res
      ?end_shrink
      ?tight_threshold
      ?tight_d:tight_spline_d
  in
  let body_spline = spline ?height ()
  and index_spline = spline ~height:index_height ()
  and thumb_spline = spline ?height:(Util.first_some thumb_height height) () in
  let east_link = Option.value ~default:thumb_spline east_link
  and west_link = Option.value ~default:thumb_spline west_link in
  let join = full_join ?slices:join_slices ?max_angle:max_join_angle ?gap_fill () in
  let thumber = Fun.const (if close_thumb then join else thumb_spline) in
  let west =
    let last_idx = Util.value_map_opt fst ~default:0 (IMap.max_binding_opt body.west) in
    fun i -> if i = last_idx then index_spline else body_spline
  in
  let north =
    let last_idx = Util.value_map_opt fst ~default:0 (IMap.max_binding_opt body.north) in
    fun i ->
      match last_idx = i, north_joins i with
      | true, _ when IMap.is_empty body.east -> body_spline
      | _, true -> join
      | _ -> if i < 2 then index_spline else body_spline
  in
  let east = Fun.const body_spline in
  let south =
    let last_idx = Util.value_map_opt fst ~default:0 (IMap.min_binding_opt body.south) in
    fun i ->
      match last_idx = i, south_joins i with
      | true, _ -> east_link
      | false, true -> join
      | _ -> body_spline
  in
  manual
    ~west
    ~north
    ~east
    ~south
    ~east_link
    ~thumb_east:thumber
    ~thumb_south:thumber
    ~thumb_west:thumber
    ~thumb_north:thumber
    ~west_link
    walls

let closed
    ?height
    ?spline_d
    ?spline_size
    ?fn
    ?min_step_dist
    ?fillet_d
    ?fillet_h
    ?corner
    ?corner_fn
    ?max_edge_res
    ?end_shrink
    ?tight_threshold
    ?tight_spline_d
    ?join_slices
    ?max_join_angle
    ?gap_fill
    ?west_link
    ?east_link
    (Walls.{ body; _ } as walls)
  =
  let join _ = full_join ?slices:join_slices ?max_angle:max_join_angle ?gap_fill ()
  and spline =
    spline
      ?height
      ?d:spline_d
      ?size:spline_size
      ?fn
      ?min_step_dist
      ?fillet_d
      ?fillet_h
      ?corner
      ?corner_fn
      ?max_edge_res
      ?end_shrink
      ?tight_threshold
      ?tight_d:tight_spline_d
      ()
  in
  let west_link = Option.value ~default:spline west_link
  and east_link = Option.value ~default:spline east_link in
  let max_key m = fst (IMap.max_binding m) in
  let north =
    let last_idx = max_key body.north in
    if IMap.cardinal walls.body.east = 0
    then fun i -> if i = last_idx then spline else join i
    else join
  in
  manual
    ~west:join
    ~north
    ~east:join
    ~south:join
    ~east_link
    ~thumb_east:join
    ~thumb_south:join
    ~thumb_west:join
    ~thumb_north:join
    ~west_link
    walls

(* let place_eyelet *)
(*     ?(fn = 16) *)
(*     ?width *)
(*     ?(bury = 0.1) *)
(*     ?(eyelet_config = Eyelet.m4_config) *)
(*     ?(relocate = false) *)
(*     ~inline *)
(*     ~outline *)
(*     loc *)
(*   = *)
(*   let half_width = *)
(*     Util.value_map_opt ~default:(eyelet_config.outer_rad +. 3.) (( *. ) 0.5) width *)
(*   in *)
(*   let len_inline = Path3.length ~closed:true inline *)
(*   and cont_inline = Path3.to_continuous ~closed:true inline *)
(*   and cont_outline = Path3.to_continuous ~closed:true outline in *)
(*   let u = Path3.continuous_closest_point ~closed:true ~n_steps:100 cont_inline loc in *)
(*   let in_pt = cont_inline u in *)
(*   let u' = Path3.continuous_closest_point ~closed:true ~n_steps:100 cont_outline in_pt in *)
(*   let shift = half_width /. len_inline in *)
(*   let lu = *)
(*     let u = u -. shift in *)
(*     if u < 0. then 1. +. u else if u > 1. then u -. 1. else u *)
(*   in *)
(*   let step = shift *. 2. /. Float.of_int fn in *)
(*   let move = V2.add V3.(to_v2 @@ ((cont_outline u' -@ in_pt) *$ bury)) in *)
(*   let ps = *)
(*     List.init (fn + 1) (fun i -> *)
(*         move @@ V2.of_v3 @@ cont_inline (lu +. (Float.of_int i *. step)) ) *)
(*   in *)
(*   let placement = *)
(*     if relocate *)
(*     then Eyelet.Normal V2.(normalize @@ ortho @@ sub (List.hd ps) (Util.last ps)) *)
(*     else Eyelet.Point (V2.of_v3 loc) *)
(*   in *)
(*   Eyelet.(make ~placement eyelet_config ps) *)

let to_scad t = t.scad
