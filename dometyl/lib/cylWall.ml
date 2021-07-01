open Base
open Scad_ml

type t =
  { scad : Model.t
  ; points : Points.t
  }

let start_chunk side (k : _ KeyHole.t) =
  let rad = k.config.thickness /. 2. in
  let cyl =
    Model.cylinder ~center:true rad k.config.outer_w
    |> Model.rotate (0., Float.pi /. 2., 0.)
  and face = KeyHole.Faces.face k.faces side in
  let r = RotMatrix.(align_exn (KeyHole.Face.direction face) (1., 0., 0.) |> to_euler)
  and t = Vec3.(KeyHole.orthogonal k side <*> (rad, rad, rad)) in
  let centre = Vec3.(face.points.centre <+> t) in
  Model.rotate r cyl |> Model.translate centre, centre

let cyl_base_points ~thickness ~width ~direction ~ortho (x, y, _) =
  let centre = x, y, 0.
  and dir_step =
    Vec3.(map (( *. ) (width /. 2.)) (normalize (mul direction (1., 1., 0.))))
  and ortho_step =
    Vec3.(map (( *. ) (thickness /. 2.)) (normalize (mul ortho (1., 1., 0.))))
  in
  Points.
    { top_left = Vec3.(add centre (add dir_step ortho_step))
    ; top_right = Vec3.(add ortho_step (sub centre dir_step))
    ; bot_left = Vec3.(add dir_step (sub centre ortho_step))
    ; bot_right = Vec3.(sub centre (add dir_step ortho_step))
    ; centre
    }

let cyl_siding
    ?(x_off = 0.)
    ?(y_off = 0.)
    ?(z_off = 2.)
    ?(d1 = 4.)
    ?(d2 = 7.)
    ?(n_steps = 10)
    side
    (key : _ KeyHole.t)
  =
  let chunk, start = start_chunk side key
  and ortho = KeyHole.orthogonal key side
  and face = KeyHole.Faces.face key.faces side in
  let z_hop = (Float.max 0. (Vec3.get_z ortho) *. key.config.thickness) +. z_off in
  let t2 =
    Vec3.(
      mul (normalize (mul ortho (1., 1., 0.))) (d1, d1, 0.) |> add (x_off, y_off, z_hop))
  and t3 =
    (* Extend down until lowest point is at xy plane. Hull will close rest of gap. *)
    let lowest_z =
      let f m (_, _, z) = Float.min m z in
      Points.fold ~f ~init:Float.max_value face.points
    in
    Vec3.(
      add
        (mul (normalize (mul ortho (1., 1., 0.))) (d2, d2, 0.))
        (x_off, y_off, -.lowest_z))
  in
  let wall = Bezier.quad_hull ~t1:(0., 0., 0.) ~t2 ~t3 ~n_steps chunk
  and points =
    let KeyHole.{ thickness; outer_w = width; _ } = key.config in
    cyl_base_points
      ~thickness
      ~width
      ~direction:(KeyHole.Face.direction face)
      ~ortho
      (Vec3.add t3 start)
  in
  let scad =
    Model.union
      [ Model.hull [ chunk; face.scad ]
      ; wall
      ; Model.hull
          [ Model.translate t3 chunk
          ; Model.polygon
              (List.map
                 ~f:Vec3.to_vec2
                 [ points.top_left; points.top_right; points.bot_right; points.bot_left ] )
            |> Model.linear_extrude ~height:0.001
          ]
      ]
  in
  { scad; points }
