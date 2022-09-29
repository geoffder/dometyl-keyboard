open! Scad_ml
open! Syntax

let last = function
  | [] -> invalid_arg "No last element in empty list."
  | h :: t -> List.fold_left (fun _ e -> e) h t

let some_if pred v = if pred then Some v else None

let map2_opt f a b =
  match a, b with
  | Some a, Some b -> Some (f a b)
  | _ -> None

let value_map_opt ~default f = function
  | Some a -> f a
  | None -> default

let first_some a b =
  match a, b with
  | Some _, _ -> a
  | None, Some _ -> b
  | None, None -> None

let merge_opt f a b =
  match a, b with
  | Some _, None -> a
  | None, Some _ -> b
  | Some a, Some b -> Some (f a b)
  | None, None -> None

let prepend_opt opt l =
  match opt with
  | Some a -> a :: l
  | None -> l

let prepend_opt_map ~f opt l =
  match opt with
  | Some a -> f a :: l
  | None -> l

let fold_init n f init =
  let rec loop acc i = if i < n then loop (f i acc) (i + 1) else acc in
  loop init 0

let rec fold_left3 f acc l1 l2 l3 =
  match l1, l2, l3 with
  | [], [], [] -> acc
  | a1 :: l1, a2 :: l2, a3 :: l3 -> fold_left3 f (f acc a1 a2 a3) l1 l2 l3
  | _, _, _ -> invalid_arg "Util.fold_left3: lists of unequal lengths"

let rec fold_left4 f acc l1 l2 l3 l4 =
  match l1, l2, l3, l4 with
  | [], [], [], [] -> acc
  | a1 :: l1, a2 :: l2, a3 :: l3, a4 :: l4 -> fold_left4 f (f acc a1 a2 a3 a4) l1 l2 l3 l4
  | _, _, _, _ -> invalid_arg "Util.fold_left4: lists of unequal lengths"

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

(* TODO: should probably add transform/row pruning and validation functions to
   scad_ml, this simple check is actually quite effective for avoiding mesh
   self-intersection. *)
let prune_transforms ?(min_dist = 0.05) ~shape = function
  | [] -> []
  | [ m ] -> [ 0, m ]
  | m0 :: transforms ->
    let s0 = Path3.affine m0 (shape 0) in
    let f (acc, i, s) m =
      let s' = Path3.affine m (shape i) in
      let plane = Path3.to_plane s in
      let valid = List.for_all (Plane.is_point_above ~eps:min_dist plane) s' in
      if valid then (i, m) :: acc, i + 1, s' else acc, i + 1, s
    in
    let transforms, _, _ = List.fold_left f ([ 0, m0 ], 1, s0) transforms in
    List.rev transforms
