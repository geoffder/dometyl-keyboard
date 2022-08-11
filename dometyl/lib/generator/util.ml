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
  if n < 3 then failwith "At least three vertices are required."
  else if n = List.length b then
    let pts = a @ b
    and sides =
      let wrap i = if i > n - 1 then i - n else i in
      List.init n ~f:(fun i -> [ i; i + n; n + wrap (i + 1); wrap (i + 1) ])
    in
    let faces =
      List.range 0 n :: List.range ~stride:(-1) ((n * 2) - 1) (n - 1) :: sides
    in
    Scad.polyhedron pts faces
  else failwith "Faces must have equal number of vertices."

let bisection_exn ?(max_iter = 100) ~tolerance ~f lower upper =
  let rec loop i a b =
    let c = (a +. b) /. 2. in
    let res = f c in
    if Float.(res = 0. || (b -. a) /. 2. < tolerance) then c
    else if i < max_iter then
      if Float.(Sign.equal (sign_exn res) (sign_exn (f a))) then loop (i + 1) c b
      else loop (i + 1) a c
    else failwith "Maximum iterations reached in bisection search."
  in
  loop 0 lower upper

let prepend_opt opt l =
  match opt with
  | Some a -> a :: l
  | None   -> l

let prepend_opt_map ~f opt l =
  match opt with
  | Some a -> f a :: l
  | None   -> l

let fill_points ?(init = []) ~n a b =
  if n < 1 then b :: a :: init
  else
    let n' = n + 1 in
    let step = Vec3.(map (fun p -> p /. Float.of_int n') (sub b a)) in
    let rec loop acc i =
      if i < n' then loop (Vec3.(a +@ (step *$ Float.of_int i)) :: acc) (i + 1)
      else b :: acc
    in
    loop (a :: init) 1

let idx_to_find = function
  | First -> Map.min_elt >> Option.map ~f:snd
  | Last  -> Map.max_elt >> Option.map ~f:snd
  | Idx i -> Fn.flip Map.find i
