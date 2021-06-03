open Base
open Scad_ml
open Sigs

type kind =
  | Well of
      { angle : float
      ; radius : float
      }
  | Fan of
      { angle : float
      ; radius : float
      }

module type Config = sig
  val centre_idx : int
  val kinds : kind list
end

module type S = sig
  include Config

  (* val place : rotater:(Core.rotate_t -> Core.pos_t -> 'a -> 'a) -> int -> 'a -> 'a *)
  val place
    :  (module Transformable with type t = KeyHole.t)
    -> int
    -> KeyHole.t
    -> KeyHole.t
end

module Make (C : Config) : S = struct
  include C

  let point = function
    | Fan { radius; _ }  -> -.radius, 0., 0.
    | Well { radius; _ } -> 0., 0., -.radius

  let theta = function
    | Well { angle; _ } -> fun i -> angle *. Int.to_float (i - centre_idx), 0., 0.
    | Fan { angle; _ }  -> fun i -> 0., 0., -.angle *. Int.to_float (i - centre_idx)

  (* let place ~rotater i transformable =
   *   let f t kind = rotater (theta kind i) (point kind) t in
   *   List.fold ~init:transformable ~f kinds *)

  (* let place (module M : Transformable with type t = KeyHole.t) i key =
   *   let f k kind = M.rotate_about_pt (theta kind i) (point kind) k in
   *   List.fold ~init:key ~f kinds *)

  (* TODO: change kind configuration to be separate spec options for well and fan.
   * Then build place according to what compination of them are present. Well goes firts,
   * so that the y transform can be cancelled out (keeping the rotation and z height), then
   * the fan can be applied to give the y and x positioning.
   *
   * Also this Transformable usage is needless, since it doesn't confer any type
   * flexibility at all. It can be done away with when I reorganize things to be less
   * module clumsy. *)
  let place (module M : Transformable with type t = KeyHole.t) i key =
    match kinds with
    | [ kind ]   -> M.rotate_about_pt (theta kind i) (point kind) key
    | [ k1; k2 ] ->
      let welled = M.rotate_about_pt (theta k1 i) (point k1) key in
      M.rotate_about_pt
        (theta k2 i)
        (point k2)
        (M.translate Util.(welled.origin <*> (0., -1., 0.)) welled)
    | _          -> key
  (* let place ~rotater i transformable =
   *   let f (origin, t) kind =
   *     let th = theta kind i
   *     and pt = point kind in
   *     (\* let origin' = Math.rotate_about_pt th  pt origin in *\)
   *     let x, y, z = origin in
   *     Stdio.printf "%.3f, %.3f, %.3f\n" x y z;
   *     Math.rotate_about_pt th pt origin, rotater th Util.(pt <-> origin) t
   *   in
   *   List.fold ~init:((0., 0., 0.), transformable) ~f kinds |> snd *)
end
