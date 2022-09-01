open! Scad_ml
open Syntax

type idx =
  | First
  | Last
  | Idx of int

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
  | First -> IMap.min_binding_opt >> Option.map snd
  | Last -> IMap.max_binding_opt >> Option.map snd
  | Idx i -> IMap.find_opt i

(* let prune_transforms ~shape = function *)
(*   | [] -> [] *)
(*   | [ m ] -> [ 0, m ] *)
(*   | m0 :: m1 :: transforms -> *)
(*     let s0 = Path3.affine m0 (shape 0) in *)
(*     let s1 = Path3.affine m1 (shape 1) in *)
(*     let vecs = List.map2 (fun a b -> V3.(b -@ a)) s0 s1 in *)
(*     let f (acc, i, s, vecs) m = *)
(*       let s' = Path3.affine m (shape i) in *)
(*       let vecs' = List.map2 (fun a b -> V3.(b -@ a)) s s' in *)
(*       let valid = *)
(*         List.for_all2 (fun a b -> Float.(abs (V3.angle a b) < pi)) vecs vecs' *)
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
          if not above then Printf.printf " <> transform #%i\n" i;
          above
        in
        List.for_all2 f s s'
      in
      if valid then (i, m) :: acc, i + 1, s' else acc, i + 1, s
      (* if valid then (i, m) :: acc, i + 1, s' else acc, i + 1, s' *)
    in
    let transforms, _, _ = List.fold_left f ([ 0, m0 ], 1, s0) transforms in
    List.rev transforms
