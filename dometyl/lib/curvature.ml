open Base
open Scad_ml

type spec =
  { radius : float
  ; angle : float
  ; tilt : float
  }

let well_point { radius; _ } = 0., 0., -.radius
let fan_point { radius; _ } = -.radius, 0., 0.
let well_theta centre_idx { angle; _ } i = angle *. Int.to_float (i - centre_idx), 0., 0.
let fan_theta centre_idx { angle; _ } i = 0., 0., -.angle *. Int.to_float (i - centre_idx)

let place ?well ?fan ~centre_idx i key =
  let well_theta' = well_theta centre_idx in
  let fan_theta' = fan_theta centre_idx in
  match well, fan with
  | Some spec, None ->
    KeyHole.rotate (0., spec.tilt, 0.) key
    |> KeyHole.rotate_about_pt (well_theta' spec i) (well_point spec)
  | None, Some spec ->
    KeyHole.rotate (0., spec.tilt, 0.) key
    |> KeyHole.rotate_about_pt (fan_theta' spec i) (fan_point spec)
  | Some w, Some f  ->
    let welled =
      KeyHole.rotate (0., w.tilt, 0.) key
      |> KeyHole.rotate_about_pt (well_theta' w i) (well_point w)
    in
    KeyHole.translate Vec3.(welled.origin <*> (0., -1., 0.)) welled
    |> KeyHole.rotate (0., f.tilt, 0.)
    |> KeyHole.rotate_about_pt (fan_theta' f i) (fan_point f)
  | None, None      -> key
