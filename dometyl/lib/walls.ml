open! Base
open! Scad_ml

(* TODO: move this somewhere, possibly Scad_ml. *)
(* Expects `a` to be clockwise from the perspective of looking at the face the vertices
 * form from the outside (see OpenScad polyhedron), and b to be in the same ordering
 * as `a` such that the edges of the prism will run between the vertices in the same
 * position. This means that the vertices in `b` should be counter-clockwise. *)
let prism_exn a b =
  let n = List.length a in
  if n < 3
  then failwith "At least three vertices are required."
  else if n = List.length b
  then (
    let pts = a @ b in
    let sides =
      let wrap i = if i > n - 1 then i - n else i in
      List.init n ~f:(fun i -> [ i; i + n; n + wrap (i + 1); wrap (i + 1) ])
    in
    let faces =
      List.range 0 n :: List.range ~stride:(-1) ((n * 2) - 1) (n - 1) :: sides
    in
    Model.polyhedron pts faces )
  else failwith "At faces must have equal number of vertices."

(* TODO: Come up with how I would like to organize the generated walls, such that
 * joining them up will be sensical.
 * - What do I want to supply the top-level functions in this module? Map of columns,
 * with wall generating closures (drop and siding) and a list of where to make each?
 * - Keep in mind that I am going to need to join up the thumb to the west side
 * and southern wall of the middle finger column. Otherwise, the thumbs functions will
 * be separate since the pattern is different there. Makes sense to have the separate
 * modules with their own make functions then. *)

(* TODO: While I am cooking up the way that Wall generation is handled, consider adusting
 * the d1/d2 values, particularly on the southern side, along with the differing
 * y-values of the end key face. Could look nice, while also maybe making the base
 * generation easier. Could even consider an x-offset on the middle finger column,
 * but that could cause it's own problems as well.
 * (Could have these adjustments as an option for the wall generation function. Either
 * give the closures or settings explicitly, or leave it up to the calculations. ) *)

let base_endpoints ~height hand (w : Wall.t) =
  let top, bot =
    match hand with
    | `Left  -> `TL, `BL
    | `Right -> `TR, `BR
  in
  [ Points.get w.points bot
  ; Points.get w.points top
  ; Wall.(Edge.point_at_z (Edges.get w.edges top) height)
  ; Wall.(Edge.point_at_z (Edges.get w.edges bot) height)
  ]

let base_steps ~n_steps starts dests =
  let norms = List.map2_exn ~f:(fun s d -> Vec3.(norm (s <-> d))) starts dests in
  let lowest_norm = List.fold ~init:Float.max_value ~f:Float.min norms in
  let adjust norm = Float.(to_int (norm /. lowest_norm *. of_int n_steps)) in
  `Ragged (List.map ~f:adjust norms)

let bez_base ?(height = 11.) ?(n_steps = 6) (w1 : Wall.t) (w2 : Wall.t) =
  let ((dx, dy, _) as dir1) = Wall.direction w1
  and dir2 = Wall.direction w2 in
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
  let dir1 = Wall.direction w1
  and dir2 = Wall.direction w2
  and dist = d, d, 0.
  and width = Vec3.(norm (w1.points.top_right <-> w1.points.bot_right)) *. scale in
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
  let dir1 = Wall.direction w1
  and dir2 = Wall.direction w2
  and dist = d, d, 0.
  and width = Vec3.(norm (w1.points.top_right <-> w1.points.bot_right)) *. scale in
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
  let dir1 = Wall.direction w1
  and dir2 = Wall.direction w2
  and ((inx, iny, _) as inward_dir) =
    Vec3.normalize Vec3.(w1.points.bot_right <-> w1.points.top_right)
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
    let w = Vec3.(norm (w1.points.bot_right <-> w1.points.top_right)) in
    let slide p = Vec3.(add p (mul dir1 (w, w, 0.))) in
    [ slide w1.points.bot_right; w1.points.bot_right; up_bot; slide up_bot ]
  and dests = base_endpoints ~height `Left w2 in
  let steps = base_steps ~n_steps starts dests
  and bezs = List.map2_exn ~f:get_bez starts dests in
  Bezier.prism_exn bezs steps

let straight_base ?(height = 11.) ?(fudge_factor = 6.) (w1 : Wall.t) (w2 : Wall.t) =
  let ((dx, dy, _) as dir1) = Wall.direction w1
  and dir2 = Wall.direction w2 in
  let fudge d =
    (* For adjustment of bottom (inside face) points to account for steep angles
     * that would otherwise cause the polyhedron to fail. Distance moved is a
     * function of how far apart the walls are along the major axis of the first. *)
    let extra =
      let diff =
        Vec3.(
          mean [ w1.points.bot_right; w1.points.top_right ]
          <-> mean [ w2.points.bot_left; w2.points.top_left ])
      in
      let major = if Float.(abs dx > abs dy) then Vec3.get_x diff else Vec3.get_y diff in
      Float.(abs (min (abs major -. fudge_factor) 0.))
    in
    Vec3.(add (mul d (extra, extra, 0.)))
  in
  let outward =
    (* away from the centre of mass, or not? *)
    Float.(
      Vec3.(norm @@ (w1.points.top_right <-> w2.points.top_left))
      > Vec3.(norm @@ (w1.points.top_right <-> w2.points.bot_left)))
  in
  let starts =
    let up_bot = Wall.Edge.point_at_z w1.edges.bot_right height in
    [ (if not outward then fudge dir1 w1.points.bot_right else w1.points.bot_right)
    ; w1.points.top_right
    ; Wall.Edge.point_at_z w1.edges.top_right height
    ; (if not outward then fudge dir1 up_bot else up_bot)
    ]
    |> List.map ~f:Vec3.(add (mul dir1 (0.1, 0.1, 0.)))
  and dests =
    let up_top = Wall.Edge.point_at_z w2.edges.top_left height
    and up_bot = Wall.Edge.point_at_z w2.edges.bot_left height
    and slide = fudge (Vec3.negate dir2) in
    [ (if outward then slide w2.points.bot_left else w2.points.bot_left)
    ; w2.points.top_left
    ; up_top
    ; (if outward then slide up_bot else up_bot)
    ]
    |> List.map ~f:Vec3.(add (mul dir2 (-0.1, -0.1, 0.)))
  in
  prism_exn starts dests

(* TODO: add a hull wall join
 * - mainly for stitching the index columns together, as high tent causes them to
 *   overlap, causing polyhedron based joining methods to fail.
 * - slide and difference the walls from themselves along their direction, then
 *   hull the resulting faces
 * - may as well default to joining the indexes like this even in skeleton mode for the
 *   simplicity. They are already so close anyway, and this way there is no fretting
 *   about placing the port holes. *)
let join_walls ?(n_steps = 6) ?(fudge_factor = 3.) (w1 : Wall.t) (w2 : Wall.t) =
  (* TODO: This actually works pretty well as a first pass, but it does not account
   * for the "swing_face" hull that has not been stored as part of Wall.t. In order
   * to fill in the area between these as well, I would need their end faces to
   * hull with, or perhaps the points of the swung face (only the top points will
   * be different) which can be used with the 0 points of the edges to create the
   * polyhedrons I would need.
   *
   * NOTE: this is actually incorrect, the bezier emerges from the swung face, so
   * I should store the key face points and swung points in Wall.t so I maintain access. *)
  let ((dx, dy, _) as dir1) = Wall.direction w1
  and dir2 = Wall.direction w2 in
  let fudge =
    let extra =
      let diff =
        Vec3.(
          mean [ w1.points.bot_right; w1.points.top_right ]
          <-> mean [ w2.points.bot_left; w2.points.top_left ])
      in
      let major, minor =
        let x, y, _ = diff in
        if Float.(abs dx > abs dy) then x, y else y, x
      in
      if Float.(abs minor > fudge_factor)
      then Float.(abs (min (abs major -. fudge_factor) 0.))
      else 0.
    in
    Vec3.(add (mul dir2 (-.extra, -.extra, 0.)))
  in
  let starts =
    Bezier.curve_rev
      ~n_steps
      ~init:(Bezier.curve ~n_steps w1.edges.bot_right)
      w1.edges.top_right
    |> List.map ~f:Vec3.(add (mul dir1 (0.1, 0.1, 0.)))
  and dests =
    Bezier.curve_rev ~n_steps ~init:(Bezier.curve ~n_steps w2.edges.bot_left) (fun step ->
        fudge @@ w2.edges.top_left step )
    |> List.map ~f:Vec3.(add (mul dir2 (-0.1, -0.1, 0.)))
  in
  prism_exn starts dests

module Body = struct
  type col =
    { north : Wall.t option
    ; south : Wall.t option
    }

  type sides =
    { west : Wall.t Map.M(Int).t
    ; east : Wall.t Map.M(Int).t
    }

  type t =
    { cols : col Map.M(Int).t
    ; sides : sides
    }

  (* let skeleton
   *     ?(d1 = 2.)
   *     ?(d2 = 5.)
   *     ~spacing
   *     ~columns
   *     ?(z_off = 0.)
   *     ?(thickness = 3.5)
   *     ?(n_steps = 5)
   *   =
   *   () *)
end

module Thumb = struct
  type key =
    { north : Wall.t option
    ; south : Wall.t option
    }

  type sides =
    { west : Wall.t option
    ; east : Wall.t option
    }

  type t =
    { keys : key Map.M(Int).t
    ; sides : sides
    }
end
