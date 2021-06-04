open Base
open Scad_ml
open Sigs

type spec =
  { radius : float
  ; angle : float
  }

module type Config = sig
  val centre_idx : int
  val well : spec option
  val fan : spec option
end

module type S = sig
  include Config

  val place
    :  (module Transformable with type t = KeyHole.t)
    -> int
    -> KeyHole.t
    -> KeyHole.t
end

module Make (C : Config) : S = struct
  include C

  let well_point { radius; _ } = 0., 0., -.radius
  let fan_point { radius; _ } = -.radius, 0., 0.
  let well_theta { angle; _ } i = angle *. Int.to_float (i - centre_idx), 0., 0.
  let fan_theta { angle; _ } i = 0., 0., -.angle *. Int.to_float (i - centre_idx)

  let place (module M : Transformable with type t = KeyHole.t) i key =
    match well, fan with
    | Some spec, None -> M.rotate_about_pt (well_theta spec i) (well_point spec) key
    | None, Some spec -> M.rotate_about_pt (fan_theta spec i) (fan_point spec) key
    | Some w, Some f  ->
      let welled = M.rotate_about_pt (well_theta w i) (well_point w) key in
      M.rotate_about_pt
        (fan_theta f i)
        (fan_point f)
        (M.translate Util.(welled.origin <*> (0., -1., 0.)) welled)
    | None, None      -> key
end
