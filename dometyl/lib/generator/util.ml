open! Base
open! Scad_ml
open Infix

type idx =
  | First
  | Last
  | Idx of int

let deg_to_rad d = d *. Float.pi /. 180.
let rad_to_deg r = r *. 180. /. Float.pi

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
    let pts = a @ b
    and sides =
      let wrap i = if i > n - 1 then i - n else i in
      List.init n ~f:(fun i -> [ i; i + n; n + wrap (i + 1); wrap (i + 1) ])
    in
    let faces =
      List.range 0 n :: List.range ~stride:(-1) ((n * 2) - 1) (n - 1) :: sides
    in
    Scad.polyhedron pts faces )
  else failwith "Faces must have equal number of vertices."

let bisection_exn ?(max_iter = 100) ~tolerance ~f lower upper =
  let rec loop i a b =
    let c = (a +. b) /. 2. in
    let res = f c in
    if Float.(res = 0. || (b -. a) /. 2. < tolerance)
    then c
    else if i < max_iter
    then
      if Float.(Sign.equal (sign_exn res) (sign_exn (f a)))
      then loop (i + 1) c b
      else loop (i + 1) a c
    else failwith "Maximum iterations reached in bisection search."
  in
  loop 0 lower upper

let prepend_opt opt l =
  match opt with
  | Some a -> a :: l
  | None -> l

let prepend_opt_map ~f opt l =
  match opt with
  | Some a -> f a :: l
  | None -> l

let fill_points ?(init = []) ~n a b =
  if n < 1
  then b :: a :: init
  else (
    let n' = n + 1 in
    let step = V3.(map (fun p -> p /. Float.of_int n') (sub b a)) in
    let rec loop acc i =
      if i < n'
      then loop (V3.(a +@ (step *$ Float.of_int i)) :: acc) (i + 1)
      else b :: acc
    in
    loop (a :: init) 1 )

let idx_to_find = function
  | First -> Map.min_elt >> Option.map ~f:snd
  | Last -> Map.max_elt >> Option.map ~f:snd
  | Idx i -> Fn.flip Map.find i

(* let prune_transforms ~shape = function *)
(*   | [] -> [] *)
(*   | [ m ] -> [ 0, m ] *)
(*   | m0 :: m1 :: transforms -> *)
(*     let s0 = Path3.affine m0 (shape 0) in *)
(*     let s1 = Path3.affine m1 (shape 1) in *)
(*     let vecs = List.map2_exn ~f:(fun a b -> V3.(b -@ a)) s0 s1 in *)
(*     let f (acc, i, s, vecs) m = *)
(*       let s' = Path3.affine m (shape i) in *)
(*       let vecs' = List.map2_exn ~f:(fun a b -> V3.(b -@ a)) s s' in *)
(*       let valid = *)
(*         List.for_all2_exn ~f:(fun a b -> Float.(abs (V3.angle a b) < pi)) vecs vecs' *)
(*       in *)
(*       if valid then (i, m) :: acc, i + 1, s', vecs' else acc, i + 1, s, vecs *)
(*       (\* if valid then (i, m) :: acc, i + 1, s', vecs' else acc, i + 1, s', vecs' *\) *)
(*     in *)
(*     let transforms, _, _, _ = List.fold ~f ~init:([], 2, s1, vecs) transforms in *)
(*     (0, m0) :: (1, m1) :: List.rev transforms *)

let prune_transforms ~shape = function
  | [] -> []
  | [ m ] -> [ 0, m ]
  | m0 :: transforms ->
    let s0 = Path3.affine m0 (shape 0) in
    let f (acc, i, s) m =
      let s' = Path3.affine m (shape i) in
      let plane = Path3.to_plane s in
      let valid =
        let f _a b =
          let above = Plane.is_point_above plane b in
          if not above then Stdio.printf " <> transform #%i\n" i;
          above
        in
        List.for_all2_exn ~f s s'
      in
      if valid then (i, m) :: acc, i + 1, s' else acc, i + 1, s
      (* if valid then (i, m) :: acc, i + 1, s' else acc, i + 1, s' *)
    in
    let transforms, _, _ = List.fold ~f ~init:([ 0, m0 ], 1, s0) transforms in
    List.rev transforms
