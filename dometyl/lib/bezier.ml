open! Base
open! Scad_ml

let zero = 0., 0., 0.

module Rotator = struct
  type t = float -> Model.t -> Model.t

  let make ?(pivot = zero) q1 q2 : t =
    let slerp = Quaternion.slerp q1 q2 in
    fun t -> Model.quaternion_about_pt (slerp t) pivot
end

let quad_weights t =
  let a = (1. -. t) **. 2.
  and b = 2. *. t *. (1. -. t)
  and c = t **. 2. in
  a, b, c

let cubic_weights t =
  let a = (1. -. t) **. 3.
  and b = 3. *. t *. ((1. -. t) **. 2.)
  and c = 3. *. (t **. 2.) *. (1. -. t)
  and d = t **. 3. in
  a, b, c, d

let quad ~p1 ~p2 ~p3 t =
  let a, b, c = quad_weights t in
  (a *. p1) +. (b *. p2) +. (c *. p3)

let cubic ~p1 ~p2 ~p3 ~p4 t =
  let a, b, c, d = cubic_weights t in
  (a *. p1) +. (b *. p2) +. (c *. p3) +. (d *. p4)

let quad_vec3 ~p1:(x1, y1, z1) ~p2:(x2, y2, z2) ~p3:(x3, y3, z3) t =
  let a, b, c = quad_weights t in
  let x = (a *. x1) +. (b *. x2) +. (c *. x3)
  and y = (a *. y1) +. (b *. y2) +. (c *. y3)
  and z = (a *. z1) +. (b *. z2) +. (c *. z3) in
  x, y, z

let cubic_vec3 ~p1:(x1, y1, z1) ~p2:(x2, y2, z2) ~p3:(x3, y3, z3) ~p4:(x4, y4, z4) t =
  let a, b, c, d = cubic_weights t in
  let x = (a *. x1) +. (b *. x2) +. (c *. x3) +. (d *. x4)
  and y = (a *. y1) +. (b *. y2) +. (c *. y3) +. (d *. y4)
  and z = (a *. z1) +. (b *. z2) +. (c *. z3) +. (d *. z4) in
  x, y, z

let curve bez dt =
  let rec loop acc t = if Float.(t <= 1.) then loop (bez t :: acc) (t +. dt) else acc in
  List.rev (loop [] 0.)

let quats q1 q2 dt =
  let slerp = Quaternion.slerp q1 q2 in
  let rec loop acc t = if Float.(t <= 1.) then loop (slerp t :: acc) (t +. dt) else acc in
  List.rev (loop [] 0.)

let hull rotator translations rotations scad =
  List.fold2_exn
    ~init:(scad, [])
    ~f:(fun (last, acc) p r ->
      let next = rotator r scad |> Model.translate p in
      next, Model.hull [ last; next ] :: acc )
    translations
    rotations
  |> fun (_, hs) -> Model.union hs

let hull' ?(rotator = fun _ s -> s) ?(translator = fun _ s -> s) dt scad =
  let transformer t = Infix.(rotator t >> translator t) in
  let rec loop last acc t =
    if Float.(t <= 1.)
    then (
      let next = transformer t scad in
      loop next (Model.hull [ last; next ] :: acc) (t +. dt) )
    else acc
  in
  Model.union @@ loop scad [] 0.

let quad_hull' ?rotator ~t1 ~t2 ~t3 ~step =
  let translator t = Model.translate (quad_vec3 ~p1:t1 ~p2:t2 ~p3:t3 t) in
  hull' ?rotator ~translator step

let cubic_hull' ?rotator ~t1 ~t2 ~t3 ~t4 ~step =
  let translator t = Model.translate (cubic_vec3 ~p1:t1 ~p2:t2 ~p3:t3 ~p4:t4 t) in
  hull' ?rotator ~translator step

let quad_hull
    ?(rotator = fun _ s -> s)
    ?(r1 = zero)
    ?(r2 = zero)
    ?(r3 = zero)
    ~t1
    ~t2
    ~t3
    ~step
  =
  let translations = curve (quad_vec3 ~p1:t1 ~p2:t2 ~p3:t3) step
  and rotations = curve (quad_vec3 ~p1:r1 ~p2:r2 ~p3:r3) step in
  hull rotator translations rotations

let cubic_hull
    ?(rotator = fun _ s -> s)
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
  let translations = curve (cubic_vec3 ~p1:t1 ~p2:t2 ~p3:t3 ~p4:t4) step
  and rotations = curve (cubic_vec3 ~p1:r1 ~p2:r2 ~p3:r3 ~p4:r4) step in
  hull rotator translations rotations
