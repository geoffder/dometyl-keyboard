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
let well_point { radius; _ } = v3 0. 0. radius
let fan_point { radius; _ } = v3 radius 0. 0.

let well_theta centre_idx { angle; _ } i =
  v3 (angle *. (Float.of_int i -. centre_idx)) 0. 0.

let fan_theta centre_idx { angle; _ } i =
  v3 0. 0. (-.angle *. (Float.of_int i -. centre_idx))

let place ?well ?fan ~centre_idx i key =
  let well_theta' = well_theta centre_idx in
  let fan_theta' = fan_theta centre_idx in
  match well, fan with
  | Some spec, None ->
    let r = well_theta' spec i in
    KeyHole.rotate (v3 0. spec.tilt (Vec3.get_x r *. spec.tilt /. -2.)) key
    |> KeyHole.rotate ~about:(well_point spec) r
  | None, Some spec ->
    KeyHole.yrot spec.tilt key
    |> KeyHole.rotate ~about:(fan_point spec) (fan_theta' spec i)
  | Some w, Some f  ->
    let welled =
      let r = well_theta' w i in
      KeyHole.rotate (v3 0. w.tilt (Vec3.get_x r *. w.tilt /. -2.)) key
      |> KeyHole.rotate ~about:(well_point w) r
    in
    KeyHole.ytrans (welled.origin.y *. -1.) welled
    |> KeyHole.yrot f.tilt
    |> KeyHole.rotate ~about:(fan_point f) (fan_theta' f i)
  | None, None      -> key

let apply ~centre_idx t =
  match t with
  | Curve { well; fan } -> place ?well ?fan ~centre_idx
  | Custom curve -> curve
  | PreTweak (curve, { well; fan }) -> fun i -> curve i >> place ?well ?fan ~centre_idx i
  | PostTweak ({ well; fan }, curve) -> fun i -> place ?well ?fan ~centre_idx i >> curve i
