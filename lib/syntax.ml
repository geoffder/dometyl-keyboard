let ( >> ) f g x = g (f x)

let zip_opt a b =
  match a, b with
  | Some a, Some b -> Some (a, b)
  | _ -> None

let ( let* ) = Option.bind
let ( and* ) = zip_opt
let ( let+ ) a f = Option.map f a
let ( and+ ) = zip_opt
