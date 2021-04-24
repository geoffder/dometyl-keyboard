open Base
open Scad_ml

type ax =
  | X
  | Y
  | Z

module type Config = sig
  val centre_idx : int
  val angle : ax * float
  val radius : ax * float
end

module type S = sig
  val place : int -> Model.t -> Model.t
end

module Make (C : Config) : S = struct
  let point =
    match C.radius with
    | X, r -> r, 0., 0.
    | Y, r -> 0., r, 0.
    | Z, r -> 0., 0., r

  let angle =
    let ax, theta = C.angle in
    let f i = theta *. Int.to_float (i - C.centre_idx) in
    match ax with
    | X -> fun i -> f i, 0., 0.
    | Y -> fun i -> 0., f i, 0.
    | Z -> fun i -> 0., 0., f i

  let place i = Util.rotate_about_pt (angle i) point
end
