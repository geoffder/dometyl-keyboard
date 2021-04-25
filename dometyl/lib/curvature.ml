open Base
open Scad_ml

type style =
  | Well
  | Fan

module type Config = sig
  val style : style
  val centre_idx : int
  val angle : float
  val radius : float
end

module type S = sig
  val place : int -> Model.t -> Model.t
end

module Make (C : Config) : S = struct
  let point =
    match C.style with
    | Fan  -> -.C.radius, 0., 0.
    | Well -> 0., 0., -.C.radius

  let angle =
    match C.style with
    | Well -> fun i -> C.angle *. Int.to_float (i - C.centre_idx), 0., 0.
    | Fan  -> fun i -> 0., 0., -.C.angle *. Int.to_float (i - C.centre_idx)

  let place i = Util.rotate_about_pt (angle i) point
end
