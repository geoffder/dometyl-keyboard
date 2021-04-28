open! Base

let quad ~p1:(x1, y1) ~p2:(x2, y2) ~p3:(x3, y3) t =
  let a = (1. -. t) **. 2.
  and b = 2. *. t *. (1. -. t)
  and c = t **. 2. in
  let x = (a *. x1) +. (b *. x2) +. (c *. x3)
  and y = (a *. y1) +. (b *. y2) +. (c *. y3) in
  x, y

let cubic ~p1:(x1, y1) ~p2:(x2, y2) ~p3:(x3, y3) ~p4:(x4, y4) t =
  let a = (1. -. t) **. 3.
  and b = 3. *. t *. ((1. -. t) **. 2.)
  and c = 3. *. (t **. 2.) *. (1. -. t)
  and d = t **. 3. in
  let x = (a *. x1) +. (b *. x2) +. (c *. x3) +. (d *. x4)
  and y = (a *. y1) +. (b *. y2) +. (c *. y3) +. (d *. y4) in
  x, y

let curve bez dt =
  let rec loop acc t = if Float.(t <= 1.) then loop (bez t :: acc) (t +. dt) else acc in
  List.rev (loop [] 0.)
