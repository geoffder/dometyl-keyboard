open Base
open Scad_ml
open! Infix

type spec =
  { radius : float
  ; angle : float
  ; tilt : float
  }

type curve =
  { well : spec option
  ; fan : spec option
  }

type 'k custom = int -> 'k KeyHole.t -> 'k KeyHole.t

type 'k t =
  | Curve of curve
  | Custom of 'k custom
  | PreTweak of 'k custom * curve
  | PostTweak of curve * 'k custom

let spec ?(tilt = 0.) ~radius angle = { radius; angle; tilt }
let curve ?well ?fan () = Curve { well; fan }
let custom f = Custom f
let pre_tweak ?well ?fan f = PreTweak (f, { well; fan })
let post_tweak ?well ?fan f = PostTweak ({ well; fan }, f)
let well_point { radius; _ } = 0., 0., -.radius
let fan_point { radius; _ } = -.radius, 0., 0.
let well_theta centre_idx { angle; _ } i = angle *. Int.to_float (i - centre_idx), 0., 0.
let fan_theta centre_idx { angle; _ } i = 0., 0., -.angle *. Int.to_float (i - centre_idx)

let place ?well ?fan ~centre_idx i key =
  let well_theta' = well_theta centre_idx in
  let fan_theta' = fan_theta centre_idx in
  match well, fan with
  | Some spec, None ->
    let r = well_theta' spec i in
    KeyHole.rotate (0., spec.tilt, Vec3.get_x r *. spec.tilt /. -2.) key
    |> KeyHole.rotate_about_pt r (well_point spec)
  | None, Some spec ->
    KeyHole.rotate (0., spec.tilt, 0.) key
    |> KeyHole.rotate_about_pt (fan_theta' spec i) (fan_point spec)
  | Some w, Some f  ->
    let welled =
      let r = well_theta' w i in
      KeyHole.rotate (0., w.tilt, Vec3.get_x r *. w.tilt /. -2.) key
      |> KeyHole.rotate_about_pt r (well_point w)
    in
    KeyHole.translate Vec3.(welled.origin <*> (0., -1., 0.)) welled
    |> KeyHole.rotate (0., f.tilt, 0.)
    |> KeyHole.rotate_about_pt (fan_theta' f i) (fan_point f)
  | None, None      -> key

let apply ~centre_idx t =
  match t with
  | Curve { well; fan } -> place ?well ?fan ~centre_idx
  | Custom curve -> curve
  | PreTweak (curve, { well; fan }) -> fun i -> curve i >> place ?well ?fan ~centre_idx i
  | PostTweak ({ well; fan }, curve) -> fun i -> place ?well ?fan ~centre_idx i >> curve i
