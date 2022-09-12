include Map.Make (Int)

let keys t = Seq.map (fun (k, _) -> k) (to_seq t)

let fold_right f t init =
  Seq.fold_left (fun acc (key, data) -> f key data acc) init (to_rev_seq t)

let filter_mapi f t =
  let g key data t =
    match f key data with
    | Some data -> add key data t
    | None -> t
  in
  fold g t empty

module ISet = Set.Make (Int)

let fold2 f init a b =
  let ks = ISet.add_seq (keys b) (ISet.add_seq (keys a) ISet.empty) in
  let g k acc =
    match find_opt k a, find_opt k b with
    | Some a, Some b -> f k (`Both (a, b)) acc
    | Some a, None -> f k (`Left a) acc
    | None, Some b -> f k (`Right b) acc
    | _ -> failwith "impossible"
  in
  ISet.fold g ks init
