open! Base
open! Scad_ml

module Rotator = struct
  type 's t = float -> 's Scad.t -> 's Scad.t

  let make ?(pivot = Vec3.zero) q1 q2 : 's t =
    let slerp = Quaternion.slerp q1 q2 in
    fun t -> Scad.quaternion_about_pt (slerp t) pivot
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

let quad_vec2 ~p1:(x1, y1) ~p2:(x2, y2) ~p3:(x3, y3) t =
  let a, b, c = quad_weights t in
  let x = (a *. x1) +. (b *. x2) +. (c *. x3)
  and y = (a *. y1) +. (b *. y2) +. (c *. y3) in
  x, y

let cubic_vec2 ~p1:(x1, y1) ~p2:(x2, y2) ~p3:(x3, y3) ~p4:(x4, y4) t =
  let a, b, c, d = cubic_weights t in
  let x = (a *. x1) +. (b *. x2) +. (c *. x3) +. (d *. x4)
  and y = (a *. y1) +. (b *. y2) +. (c *. y3) +. (d *. y4) in
  x, y

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

let curve_rev ?(init = []) ?(n_steps = 10) bez =
  let dt = 1. /. Float.of_int n_steps in
  let rec loop acc i t =
    if i <= n_steps then loop (bez t :: acc) (i + 1) (t +. dt) else acc
  in
  loop init 0 0.

let curve ~n_steps bez = List.rev (curve_rev ~init:[] ~n_steps bez)

let quats q1 q2 dt =
  let slerp = Quaternion.slerp q1 q2 in
  let rec loop acc t = if Float.(t <= 1.) then loop (slerp t :: acc) (t +. dt) else acc in
  List.rev (loop [] 0.)

let hull ?(rotator = fun _ s -> s) ?(translator = fun _ s -> s) n_steps scad =
  let dt = 1. /. Float.of_int n_steps
  and transformer t = Infix.(rotator t >> translator t) in
  let rec loop last acc t =
    if Float.(t <= 1.)
    then (
      let next = transformer t scad in
      loop next (Scad.hull [ last; next ] :: acc) (t +. dt) )
    else acc
  in
  Scad.union @@ loop scad [] 0.

let quad_hull ?rotator ~t1 ~t2 ~t3 ~n_steps =
  let translator t = Scad.translate (quad_vec3 ~p1:t1 ~p2:t2 ~p3:t3 t) in
  hull ?rotator ~translator n_steps

let cubic_hull ?rotator ~t1 ~t2 ~t3 ~t4 ~n_steps =
  let translator t = Scad.translate (cubic_vec3 ~p1:t1 ~p2:t2 ~p3:t3 ~p4:t4 t) in
  hull ?rotator ~translator n_steps

(* Provide beziers in clockwise order, from the perspective of looking at the
 * face created by the first control points from the outside (see OpenScad polyhedron). *)
let prism_exn bezs n_steps =
  let n_bez = List.length bezs in
  let n_steps' =
    match n_steps with
    | `Uniform n -> List.init ~f:(fun _ -> n) n_bez
    | `Ragged l  ->
      if List.length l = n_bez
      then l
      else failwith "Length of Ragged n_steps must match bezs."
  in
  let pts_per = Array.of_list_map ~f:(( + ) 1) n_steps' in
  if n_bez < 3
  then failwith "At least three beziers are required."
  else (
    let pts =
      List.fold2_exn
        ~init:[]
        ~f:(fun ps b n -> curve_rev ~init:ps ~n_steps:n b)
        bezs
        n_steps'
      |> List.rev
    in
    let starts = Array.folding_map ~init:0 ~f:(fun s n -> s + n, s) pts_per in
    let sides =
      let face left_i right_i =
        let left_n = pts_per.(left_i)
        and left_start = starts.(left_i)
        and right_start = starts.(right_i) in
        let f i =
          if i < left_n then left_start + left_n - 1 - i else right_start + (i - left_n)
        in
        List.init ~f (left_n + pts_per.(right_i))
      in
      let wrap i = if i < 0 then n_bez + i else i in
      List.init n_bez ~f:(fun i -> face i (wrap (i - 1)))
    in
    let faces =
      Array.to_list starts
      :: Array.foldi ~init:[] ~f:(fun i acc s -> (s + pts_per.(i) - 1) :: acc) starts
      :: sides
    in
    Scad.polyhedron pts faces )

let prism bezs dt =
  try Ok (prism_exn bezs dt) with
  | Failure e -> Error e
