open! Base
open! Scad_ml

(* TODO: Come up with how I would like to organize the generated walls, such that
 * joining them up will be sensical.
 * - What do I want to supply the top-level functions in this module? Map of columns,
 * with wall generating closures (drop and siding) and a list of where to make each?
 * - Keep in mind that I am going to need to join up the thumb to the west side
 * and southern wall of the middle finger column. Otherwise, the thumbs functions will
 * be separate since the pattern is different there. Makes sense to have the separate
 * modules with their own make functions then. *)
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

  let base ?(height = 11.) ?(n_steps = 6) (w1 : Wall.t) (w2 : Wall.t) =
    let ((dx, dy, _) as dir1) = Points.direction w1.points
    and dir2 = Points.direction w2.points in
    let mask = if Float.(abs dx > abs dy) then 1., 0., 0. else 0., 1., 0. in
    let get_bez start dest =
      let diff = Vec3.(dest <-> start) in
      let p1 = Vec3.(start <+> mul dir1 (0.5, 0.5, 0.)) (* fudge for union *)
      and p2 = Vec3.(start <+> mul mask diff)
      (* and p3 = Vec3.(dest <+> mul dir2 (0.0002, 0.0002, 0.)) in *)
      and p3 = Vec3.(dest <-> mul dir2 (0.5, 0.5, 0.)) in
      Bezier.quad_vec3 ~p1 ~p2 ~p3
    in
    let h = 0., 0., height in
    let starts =
      let mid = Vec3.(map (( *. ) 0.5) (w1.points.top_right <+> w1.points.bot_right)) in
      Vec3.
        [ w1.points.bot_right
        ; w1.points.top_right
        ; map (( *. ) 0.5) (w1.points.top_right <+> mid) <+> h
        ; map (( *. ) 0.5) (w1.points.bot_right <+> mid) <+> h
          (* ; w1.points.top_right <+> h
           * ; w1.points.bot_right <+> h *)
        ]
    in
    let dests =
      let mid = Vec3.(map (( *. ) 0.5) (w2.points.top_left <+> w2.points.bot_left)) in
      Vec3.
        [ w2.points.bot_left
        ; w2.points.top_left
        ; map (( *. ) 0.5) (w2.points.top_left <+> mid) <+> h
        ; map (( *. ) 0.5) (w2.points.bot_left <+> mid) <+> h
          (* ; w2.points.top_left <+> h
           * ; w2.points.bot_left <+> h *)
        ]
    in
    let steps =
      let norms = List.map2_exn ~f:(fun s d -> Vec3.(norm (s <-> d))) starts dests in
      let lowest_norm = List.fold ~init:Float.max_value ~f:Float.min norms in
      let adjust norm = Float.(to_int (norm /. lowest_norm *. of_int n_steps)) in
      `Ragged (List.map ~f:adjust norms)
    (* `Uniform n_steps *)
    and bezs = List.map2_exn ~f:get_bez starts dests in
    Bezier.prism_exn bezs steps
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
