open! Base
open! Scad_ml

let zero = 0., 0., 0.

let quad ~p1:(x1, y1, z1) ~p2:(x2, y2, z2) ~p3:(x3, y3, z3) t =
  let a = (1. -. t) **. 2.
  and b = 2. *. t *. (1. -. t)
  and c = t **. 2. in
  let x = (a *. x1) +. (b *. x2) +. (c *. x3)
  and y = (a *. y1) +. (b *. y2) +. (c *. y3)
  and z = (a *. z1) +. (b *. z2) +. (c *. z3) in
  x, y, z

let cubic ~p1:(x1, y1, z1) ~p2:(x2, y2, z2) ~p3:(x3, y3, z3) ~p4:(x4, y4, z4) t =
  let a = (1. -. t) **. 3.
  and b = 3. *. t *. ((1. -. t) **. 2.)
  and c = 3. *. (t **. 2.) *. (1. -. t)
  and d = t **. 3. in
  let x = (a *. x1) +. (b *. x2) +. (c *. x3) +. (d *. x4)
  and y = (a *. y1) +. (b *. y2) +. (c *. y3) +. (d *. y4)
  and z = (a *. z1) +. (b *. z2) +. (c *. z3) +. (d *. z4) in
  x, y, z

let curve bez dt =
  let rec loop acc t = if Float.(t <= 1.) then loop (bez t :: acc) (t +. dt) else acc in
  List.rev (loop [] 0.)

let hull ?(pivot = zero) translations rotations scad =
  List.fold2_exn
    ~init:(scad, [])
    ~f:(fun (last, acc) p r ->
      let next = Model.rotate_about_pt r pivot scad |> Model.translate p in
      next, Model.hull [ last; next ] :: acc )
    translations
    rotations
  |> fun (_, hs) -> Model.union hs

let quad_hull ?(pivot = zero) ?(r1 = zero) ?(r2 = zero) ?(r3 = zero) ~t1 ~t2 ~t3 ~step =
  let translations = curve (quad ~p1:t1 ~p2:t2 ~p3:t3) step
  and rotations = curve (quad ~p1:r1 ~p2:r2 ~p3:r3) step in
  hull ~pivot translations rotations

let cubic_hull
    ?(pivot = zero)
    ?(r1 = zero)
    ?(r2 = zero)
    ?(r3 = zero)
    ?(r4 = zero)
    ~t1
    ~t2
    ~t3
    ~t4
    ~step
  =
  let translations = curve (cubic ~p1:t1 ~p2:t2 ~p3:t3 ~p4:t4) step
  and rotations = curve (cubic ~p1:r1 ~p2:r2 ~p3:r3 ~p4:r4) step in
  hull ~pivot translations rotations
