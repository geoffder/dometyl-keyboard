open! Scad_ml
open! Syntax

type t =
  { scad : Scad.d3
  ; outline : Path3.t
  ; inline : Path3.t
  }
[@@deriving scad]

(* TODO:
   - break bezier spline and linear connectors into separate functions, and the
    work that they share into a higher level helper. Another router function
    can handle deciding which one to use.
   - better angle logic for the linear connector (better knobs for how to do
    rotation stepping, and handling the >90degrees cases)z
   - elbow bezier case (subcase of bezier spline?), largely as an option
    solution for the southern pinky to ring connection *)

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
  let zip a b = List.map2 (fun a b -> a, b) a b in
  List.rev_append (zip bot_edge corners) (zip top_edge corners)
  |> Path3.Round.mix
  |> Path3.roundover ?fn

let fillet ?(style = `Scale) ~d ~h rows =
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
    match style with
    | `Scale ->
      let h =
        match h with
        | `Rel h -> h
        | `Abs h -> h /. (Path3.bbox (List.hd rows)).max.z
      in
      fun u row ->
        let u = if u > 0.5 then 1. -. u else u in
        Path3.scale (v3 1. 1. (1. -. (ez u *. h))) row
    | `Shift ->
      let range =
        match h with
        | `Rel h -> (Path3.bbox (List.hd rows)).max.z *. h
        | `Abs h -> h
      in
      fun u row ->
        let u = if u > 0.5 then 1. -. u else u in
        let diff = ez u *. range in
        let shift p = if p.z > diff +. 0.1 then V3.ztrans (-.diff) p else p in
        List.map shift row
  in
  List.map2 f rel_dists rows

let id = ref 0

(* TODO: params to add

   - ending profile shrink parameter
   - fillet parameters
   - d adjustment parameters, such as min distance / angle ratio, or some way to
     accomplish similar (give more control over d as well with variant that
     takes a function?)
   - maximum edge "resolution" (required for dedup)
   - transform pruning epsilon
   - bezier spline size *)
let spline_base
    ?(height = 11.)
    ?(d = 1.)
    ?(corner = Path3.Round.bez (`Joint 1.))
    ?(corner_fn = 6)
    ?(fn = 64)
    (* ?(max_edge_res = 0.5) *)
      (w1 : Wall.t)
    (w2 : Wall.t)
  =
  let max_edge_res = 0.5 in
  let dir1 = Wall.foot_direction w1
  and dir2 = Wall.foot_direction w2 in
  let end_edges ?(shrink = 0.) ?frac left =
    let edger (w : Wall.t) corner =
      let edge = w.drawer corner in
      let u =
        Path3.continuous_closest_point
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
    plane, dedup bot, dedup top
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
  let a = round bot_l top_l
  and a' = round bot_l' top_l'
  and b = round bot_r top_r
  and b' = round bot_r' top_r' in
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
  let min_spline_dist = d *. 4. in
  let dist = V2.(distance (v p0.x p0.y) (v p3.x p3.y)) in
  let min_spline_ratio = 6. in
  let ang_ratio = dist /. Float.abs ang /. Float.pi in
  Printf.printf "id %i: ang = %f; ratio = %f\n" !id ang ang_ratio;
  let fillet_d = `Rel 0.2 in
  let fillet_h = `Rel 0.3 in
  let step = 1. /. Float.of_int fn in
  let transition i = List.map2 (fun a b -> V3.lerp a b (Float.of_int i *. step)) a' b' in
  (* TODO: experiment with automatic "out" d adjustment as well as size as
    mentioned above. I think I can remove the need for most uses of the
    linear/rot connectors this way and make it more of an option rather than a
     necessity. The auto function could optionally take bow params to only be
    used when one is encountered for example (e.g.
     - decrease out d when harsh angles or short distances are detected
     - increasing d, when bowing is detected
      *)
  let out =
    (* TODO: possible to roughly calculate an out distance based on these
    factors rather than hardcoding to 0.1 or some other small number? Or is the
    iterative "least pruned" approach I was thinking of the best? *)
    if (dist < min_spline_dist || ang_ratio < min_spline_ratio) && V3.angle dir1 dir2 < 3.
    then 0.1
    else d
  in
  (* TODO: what about trying a range of out/size values and keeping the one that
    drops the least? *)
  let transforms =
    let path =
      let p1 = V3.(p0 +@ (dir1 *$ -.out))
      and p2 = V3.(p3 +@ (dir2 *$ out)) in
      (* TODO: related to above, the size param of of_path may be useful for
           automatic mesh intersection avoidance (e.x. allow more "S" shape when tight). *)
      Bezier3.curve ~fn
      @@ Bezier3.of_path ~size:(`Abs [ out *. 0.1; 8.; out *. 0.1 ]) [ p0; p1; p2; p3 ]
    in
    Util.prune_transforms ~shape:transition @@ Path3.to_transforms ~mode:`NoAlign path
  in
  let in_start', out_start' = base_points a' in
  let scad =
    let rows =
      List.map (fun (i, m) -> Path3.affine m (transition i)) transforms
      |> fillet ~style:`Scale ~d:fillet_d ~h:fillet_h
    in
    let slices, last_row =
      (* while grabbing last row, make a slice count list (drop one to match
            number of transitions) *)
      let slices, r = List.fold_left (fun (zs, _) r -> 0 :: zs, r) ([ 5 ], []) rows in
      `Mix (5 :: List.tl slices), r
    and round_edges (_, bot, top) =
      let n = Int.(max edge_len (max (List.length bot) (List.length top))) in
      let subdiv = Path3.subdivide ~freq:(`N (n, `ByLen)) in
      round (subdiv bot) (subdiv top)
    in
    let start = round_edges @@ end_edges ~shrink:0.025 ~frac:0.01 true
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
      round_edges @@ end_edges ~shrink:0.025 ~frac:(Float.min 0.99 rel) false
    in
    Mesh.to_scad @@ Mesh.skin ~slices (List.append (start :: rows) [ finish ])
  and inline = List.map (fun (_, m) -> V3.affine m in_start') transforms
  and outline = List.map (fun (_, m) -> V3.affine m out_start') transforms in
  let () =
    let show = Path3.show_points (Fun.const (Scad.sphere 0.4)) >> Scad.color Color.Red in
    Scad.union [ scad; show a; show b ]
    |> Scad.to_file (Printf.sprintf "conn_%i.scad" !id)
  in
  id := !id + 1;
  { scad; inline; outline }

let join_walls (w1 : Wall.t) (w2 : Wall.t) =
  let dir1 = Wall.foot_direction w1
  and dir2 = Wall.foot_direction w2 in
  let d = V3.distance w2.start.top_left w1.start.top_right in
  let ang =
    V3.angle
      (V3.neg dir1)
      V3.(normalize (w2.start.top_left -@ w1.start.top_right) *@ v 1. 1. 0.)
    *. outward_sign w1 w2
  in
  let ang_ratio = d /. Float.abs ang /. Float.pi in
  let outward = outward_sign w1 w2 > 1. in
  ignore (ang_ratio, outward, dir1, dir2);
  let dedup =
    Path3.deduplicate_consecutive ~keep:`FirstAndEnds ~eq:(V3.approx ~eps:1e-1)
  in
  (* TODO: rather than use the ends, I should calculate a factor to slide the
    more outward end along the wall (given to drawer) that will improve the
    angle when it is too sharp (razor thin / intersecting mesh) *)
  let a_bot = dedup (w1.drawer (`B 0.99))
  and a_top = dedup (w1.drawer (`T 0.99))
  and b_bot = dedup (w2.drawer (`B 0.01))
  and b_top = dedup (w2.drawer (`T 0.01)) in
  let a = List.append a_top (List.rev a_bot)
  and b = List.append b_top (List.rev b_bot) in
  let scad = Mesh.skin_between ~slices:32 a b |> Mesh.to_scad in
  let () =
    let show = Path3.show_points (Fun.const (Scad.sphere 0.4)) >> Scad.color Color.Red in
    Scad.union [ scad; show a; show b ]
    |> Scad.to_file (Printf.sprintf "conn_%i.scad" !id)
  in
  id := !id + 1;
  Util.{ scad; outline = [ last a_top; last b_top ]; inline = [ last a_bot; last b_bot ] }
(* let join_walls *)
(*     ?(n_steps = `Flat 6) *)
(*     ?(fudge_factor = 3.) *)
(*     ?(overlap_factor = 1.2) *)
(*     (w1 : Wall.t) *)
(*     (w2 : Wall.t) *)
(*   = *)
(*   let ({ x = dx; y = dy; z = _ } as dir1) = Wall.foot_direction w1 *)
(*   and dir2 = Wall.foot_direction w2 *)
(*   and n_steps = *)
(*     let summit (w : Wall.t) = *)
(*       Points.fold (fun m p -> Float.max (V3.get_z p) m) ~init:Float.min_value w.start *)
(*     in *)
(*     Wall.Steps.to_int n_steps (Float.max (summit w1) (summit w2)) *)
(*   in *)
(*   let major_diff, minor_diff = *)
(*     let { x; y; z = _ } = *)
(*       V3.( *)
(*         mid w1.foot.bot_right w1.foot.top_right -@ mid w2.foot.bot_left w2.foot.top_left) *)
(*     in *)
(*     if (abs dx > abs dy) then x, y else y, x *)
(*   in *)
(*   let outward = *)
(*     (\* away from the centre of mass, or not? *\) *)
(*     ( *)
(*       V3.distance w1.foot.top_right w2.foot.top_left *)
(*       > V3.distance w1.foot.top_right w2.foot.bot_left) *)
(*   and overhang = *)
(*     (\* Obtuse angle between wall top difference and the foot difference indicates *)
(*        that the start wall is likely dodging the column below. If this is a corner, *)
(*        don't flag. *\) *)
(*     let foot_diff = V3.(w1.foot.top_right -@ w2.foot.top_right) *)
(*     and top_diff = V3.(mul (w1.start.top_right -@ w2.start.top_left) (v3 1. 1. 0.)) in *)
(*     let mag = V3.norm top_diff in *)
(*     if (mag > 0.1 && V3.dot foot_diff top_diff < 0.) then Some mag else None *)
(*   in *)
(*   (\* Move the start or destination points along the outer face of the wall to improve angle. *\) *)
(*   let fudge start = *)
(*     if (not outward) && not start *)
(*     then ( *)
(*       let extra = *)
(*         if (V3.(get_z w1.start.top_right > get_z w2.start.top_left)) *)
(*         then (abs (max (fudge_factor -. major_diff) 0.)) *)
(*         else 0. *)
(*       in *)
(*       V3.(add (mul dir2 (v3 (-.extra) (-.extra) 0.))) ) *)
(*     else Fun.id *)
(*   and overlap = *)
(*     match overhang with *)
(*     | Some over -> over *. overlap_factor *)
(*     | None -> *)
(*       let major_ax = if (abs dx > abs dy) then dx else dy *)
(*       and intersect = Points.overlapping_bounds w1.foot w2.foot in *)
(*       if ( *)
(*            ( (not (Sign.equal (sign major_diff) (sign major_ax))) *)
(*            && abs major_diff > abs minor_diff ) *)
(*            || intersect > 0.) *)
(*       then ( *)
(*         let rough_area = *)
(*           V3.( *)
(*             distance w1.foot.top_right w1.foot.top_left *)
(*             *. distance w1.foot.top_right w1.foot.bot_right) *)
(*         in *)
(*         Float.max (Float.abs major_diff) (intersect *. rough_area) *. overlap_factor ) *)
(*       else 0.01 *)
(*     (\* If the walls are overlapping, move back the start positions to counter. *\) *)
(*   in *)
(*   (\* HACK: The way I am using the overhang flag here seemed to partially rescue one case, *)
(*      but I am not confident that it is a solid fix. *\) *)
(*   let top_start, starts = *)
(*     let shove = V3.(add (mul dir1 (v3 overlap overlap 0.))) in *)
(*     let top = *)
(*       w1.edges.top_right 0. *)
(*       |> fudge true *)
(*       |> (if Option.is_some overhang then Fun.id else shove) *)
(*       |> w1.edge_drawer.top *)
(*     in *)
(*     ( top *)
(*     , Bezier3.curve *)
(*         ~rev:true *)
(*         ~fn:n_steps *)
(*         ~init: *)
(*           (Bezier3.curve *)
(*              ~fn:n_steps *)
(*              (w1.edge_drawer.bot (shove @@ w1.edges.bot_right 0.)) ) *)
(*         top ) *)
(*   and top_dest, dests = *)
(*     let shove = V3.(add (mul dir2 (v3 (-.overlap) (-.overlap) 0.))) in *)
(*     let top = *)
(*       w2.edges.top_left 0. *)
(*       |> fudge false *)
(*       |> (if Option.is_some overhang then Fun.id else shove) *)
(*       |> w2.edge_drawer.top *)
(*     in *)
(*     ( top *)
(*     , Bezier3.curve *)
(*         ~rev:true *)
(*         ~fn:n_steps *)
(*         ~init: *)
(*           (Bezier3.curve ~fn:n_steps (w2.edge_drawer.bot (shove @@ w2.edges.bot_left 0.))) *)
(*         top ) *)
(*   in *)
(*   let wedge = *)
(*     (\* Fill in the volume between the "wedge" hulls that are formed by swinging the *)
(*      * key face and moving it out for clearance prior to drawing the walls. *\) *)
(*     Util.prism *)
(*       (List.map *)
(*          V3.(add (mul (Wall.start_direction w1) (v3 overlap overlap 0.))) *)
(*          [ w1.start.top_right *)
(*          ; w1.start.bot_right *)
(*          ; w1.edges.bot_right 0. *)
(*          ; top_start 0.001 *)
(*          ] ) *)
(*       (List.map *)
(*          V3.(add (mul (Wall.start_direction w2) (v3 (-0.01) (-0.01) 0.))) *)
(*          [ w2.start.top_left; w2.start.bot_left; w2.edges.bot_left 0.; top_dest 0.001 ] ) *)
(*   in *)
(*   { scad = Scad.union [ Util.prism starts dests; wedge ] *)
(*   ; outline = [ fudge true w1.foot.top_right; fudge false w2.foot.top_left ] *)
(*   ; inline = *)
(*       [ V3.(add (mul dir1 (v3 overlap overlap 0.)) w1.foot.bot_right) *)
(*       ; V3.(add (mul dir2 (v3 (-.overlap) (-.overlap) 0.)) w2.foot.bot_left) *)
(*       ] *)
(*   } *)

type config =
  | FullJoin of
      { fudge_factor : float option
      ; overlap_factor : float option
      }
  | Spline of
      { height : float option
      ; d : float option
      ; fn : int option
      ; corner_fn : int option
      ; corner : Path3.Round.corner option
      }

let full_join ?fudge_factor ?overlap_factor () = FullJoin { fudge_factor; overlap_factor }

let spline ?height ?d ?fn ?corner_fn ?corner () =
  Spline { height; d; fn; corner_fn; corner }

let connect = function
  | Spline { height; d; fn; corner_fn; corner } ->
    spline_base ?height ?d ?fn ?corner_fn ?corner
  | FullJoin _ -> join_walls

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
  ignore (south, thumb_swoop, west, north, east);
  List.map clockwise_union [ west; north; east; south; thumb_swoop ] |> clockwise_union
(* List.map clockwise_union [ west; north; east ] |> clockwise_union *)
(* List.map clockwise_union [ west; north; east; south ] |> clockwise_union *)
(* List.map clockwise_union [ west; north; east; thumb_swoop ] |> clockwise_union *)

let skeleton
    ?(index_height = 13.)
    ?height
    ?spline_d
    ?fn
    ?corner_fn
    ?corner
    ?fudge_factor
    ?overlap_factor
    ?thumb_height
    ?east_link
    ?west_link
    ?(north_joins = fun i -> i < 2)
    ?(south_joins = fun _ -> false)
    ?(close_thumb = false)
    (Walls.{ body; thumb } as walls)
  =
  let body_spline = spline ?height ?d:spline_d ?fn ?corner_fn ?corner ()
  and index_spline = spline ~height:index_height ?d:spline_d ?fn ?corner_fn ?corner ()
  and thumb_spline =
    spline
      ?height:(Util.first_some thumb_height height)
      ?d:spline_d
      ?fn
      ?corner_fn
      ?corner
      ()
  in
  let east_link = Option.value ~default:thumb_spline east_link
  and west_link = Option.value ~default:thumb_spline west_link in
  let join = full_join ?fudge_factor ?overlap_factor ()
  and thumb_corner =
    if close_thumb then full_join ~fudge_factor:0. ?overlap_factor () else thumb_spline
  in
  let thumb_mid = if close_thumb then join else thumb_spline in
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
  let thumb_side last_idx i = if i = last_idx then thumb_corner else thumb_mid
  and idx elt = Util.value_map_opt fst ~default:0 elt in
  manual
    ~west
    ~north
    ~east
    ~south
    ~east_link
    ~thumb_east:(thumb_side (idx @@ IMap.max_binding_opt thumb.east))
    ~thumb_south:(thumb_side (idx @@ IMap.min_binding_opt thumb.south))
    ~thumb_west:(thumb_side (idx @@ IMap.min_binding_opt thumb.west))
    ~thumb_north:(thumb_side (idx @@ IMap.max_binding_opt thumb.north))
    ~west_link
    walls

let closed
    ?fudge_factor
    ?overlap_factor
    ?height
    ?spline_d
    ?fn
    ?corner_fn
    ?corner
    ?(west_link = full_join ~fudge_factor:0. ())
    ?(east_link = spline ())
    (Walls.{ body; _ } as walls)
  =
  let join = full_join ?fudge_factor ?overlap_factor ()
  and corner_join = full_join ~fudge_factor:0. ?overlap_factor ()
  and spline = spline ?height ?d:spline_d ?fn ?corner_fn ?corner () in
  let side last_idx i = if i = last_idx then corner_join else join
  and max_key m = fst (IMap.max_binding m) in
  let north =
    let last_idx = max_key body.north in
    if IMap.cardinal walls.body.east = 0
    then fun i -> if i = last_idx then spline else join
    else side last_idx
  in
  manual
    ~west:(side (max_key body.west))
    ~north
    ~east:(side 0)
    ~south:(fun _ -> join)
    ~east_link
    ~thumb_east:(fun _ -> corner_join)
    ~thumb_south:(side 0)
    ~thumb_west:(fun _ -> corner_join)
    ~thumb_north:(fun _ -> join)
    ~west_link
    walls

let to_scad t = t.scad
