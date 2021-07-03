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

(* TODO:
 * - cubic bezier base function for use on easter side when there is no siding
 * there (should be the default, will probably look nicer).
 * - full wall connection function (try drawing a face for each using the bezier
 * edges, then pass these to the basic prism generator to try and "hull" them).
 * - if the polyhedron full connection doesn't work due to overlapping points at
 * the top, then try making faces by direction sliding subtraction (carried by Wall.t?)
 * then hull those. *)
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

  let bez_base ?(height = 11.) ?(n_steps = 6) (w1 : Wall.t) (w2 : Wall.t) =
    let direction Points.{ top_left; top_right; _ } =
      Vec3.normalize Vec3.(top_left <-> top_right)
    in
    let ((dx, dy, _) as dir1) = direction w1.points
    and dir2 = direction w2.points in
    let mask = if Float.(abs dx > abs dy) then 1., 0., 0. else 0., 1., 0. in
    let get_bez start dest =
      let diff = Vec3.(dest <-> start) in
      let p1 = Vec3.(start <+> mul dir1 (0.01, 0.01, 0.)) (* fudge for union *)
      and p2 = Vec3.(start <+> mul mask diff)
      and p3 = Vec3.(dest <-> mul dir2 (0.01, 0.01, 0.)) in
      Bezier.quad_vec3 ~p1 ~p2 ~p3
    in
    let starts =
      [ w1.points.bot_right
      ; w1.points.top_right
      ; Wall.Edge.point_at_z w1.edges.top_right height
      ; Wall.Edge.point_at_z w1.edges.bot_right height
      ]
    in
    let dests =
      [ w2.points.bot_left
      ; w2.points.top_left
      ; Wall.Edge.point_at_z w2.edges.top_left height
      ; Wall.Edge.point_at_z w2.edges.bot_left height
      ]
    in
    let steps =
      let norms = List.map2_exn ~f:(fun s d -> Vec3.(norm (s <-> d))) starts dests in
      let lowest_norm = List.fold ~init:Float.max_value ~f:Float.min norms in
      let adjust norm = Float.(to_int (norm /. lowest_norm *. of_int n_steps)) in
      `Ragged (List.map ~f:adjust norms)
    and bezs = List.map2_exn ~f:get_bez starts dests in
    Bezier.prism_exn bezs steps

  let straight_base ?(height = 11.) (w1 : Wall.t) (w2 : Wall.t) =
    (* TODO: further improve resiliency to steep angles. May not be able to avoid
     * externally visible (top side) entry of base into wall if I want to make sure
     * that extreme splay angles or situations like the thumb connection don't break
     * it. Perhaps, have a flag for if the extreme measure is needed? (then move the
     * top point on the dest wall so that the wall can be thicker )*)
    let direction Points.{ top_left; top_right; _ } =
      Vec3.normalize Vec3.(top_left <-> top_right)
    in
    let ((dx, dy, _) as dir1) = direction w1.points
    and dir2 = direction w2.points in
    let fudge d =
      let extra =
        let diff =
          Vec3.(
            mean [ w1.points.bot_right; w1.points.top_right ]
            <-> mean [ w2.points.bot_left; w2.points.top_left ])
        in
        let major =
          if Float.(abs dx > abs dy) then Vec3.get_x diff else Vec3.get_y diff
        in
        Float.(abs (min (abs major -. 5.) 0.))
      in
      Vec3.(add (mul d (extra, extra, 0.)))
    in
    let outward =
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
      [ ( if outward
        then slide w2.points.bot_left
        else
          w2.points.bot_left
          (* ; (if not outward then slide w2.points.top_left else w2.points.top_left)
           * ; (if not outward then slide up_top else up_top) *) )
      ; w2.points.top_left
      ; up_top
      ; (if outward then slide up_bot else up_bot)
      ]
      |> List.map ~f:Vec3.(add (mul dir2 (-0.1, -0.1, 0.)))
    in
    prism_exn starts dests
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
