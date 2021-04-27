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
  include Config

  val place : rotater:(Core.rotate_t -> Core.pos_t -> 'a) -> int -> 'a
end

module Make (C : Config) : S = struct
  include C

  let point =
    match style with
    | Fan  -> -.radius, 0., 0.
    | Well -> 0., 0., -.radius

  let theta =
    match style with
    | Well -> fun i -> angle *. Int.to_float (i - centre_idx), 0., 0.
    | Fan  -> fun i -> 0., 0., -.angle *. Int.to_float (i - centre_idx)

  let place ~rotater i = rotater (theta i) point
end
