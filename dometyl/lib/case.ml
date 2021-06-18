open! Base
open! Scad_ml
open! Infix

let keyhole = KeyHole.make Niz.hole_config

(* TODO: consider how columns are basically assumed to be oriented along the y-axis,
 * and how things like bez_wall would break if there were to be any fanning or
 * splaying going on in the columns. Columns are never going to be fanned, so having
 * to provide a configuration feels silly already. Can I avoid exposing fan where
 * unneccesary while still reusing curvature code effectively? Think about it when
 * designing the future type/module overhaul.
 *
 * Should consider how to make the wall attachments and their rotation/translation
 * settings could be more robust to allow splaying as a future possibility. *)
let n_rows = 3
let centre_idx = 1

let column =
  Column.make
    ~n_keys:n_rows
    ~curve:Curvature.(place ~well:{ angle = Float.pi /. 12.; radius = 85. } ~centre_idx)
    keyhole

let well_column spec =
  Column.make ~n_keys:n_rows ~curve:Curvature.(place ~well:spec ~centre_idx) keyhole

let thumb =
  Column.(
    make
      ~join_ax:`EW
      ~n_keys:3
      ~curve:
        Curvature.(
          place
            ~well:{ angle = Float.pi /. 12.; radius = 85. }
            ~fan:{ angle = Float.pi /. 12.; radius = 85. }
            ~centre_idx:1)
      (KeyHole.rotate (0., 0., Float.pi /. 2.) keyhole)
    (* orient along x-axis *)
    |> rotate (0., 0., Float.pi /. -2.))

module Plate = struct
  type 'k t =
    { scad : Model.t
    ; columns : 'k Column.t Map.M(Int).t
    ; thumb : 'k Column.t
    }

  let n_cols = 5
  let spacing = 2.
  let centre_col = 2
  let tent = Float.pi /. 6.
  let thumb_offset = 7., -50., -3.
  let thumb_angle = Float.(0., pi /. -4., pi /. 5.)
  let clearance = 7.

  (* TODO: tune *)
  let offset_lookup = function
    | 2 -> 0., 6., -6. (* middle *)
    | 3 -> 0., 3., -2. (* ring *)
    | i when i >= 4 -> 0., -12., 6. (* pinky *)
    | _ -> 0., 0., 0.

  let well_lookup = function
    (* | i when i >= 4 -> Curvature.{ angle = Float.pi /. 9.; radius = 60. } (\* pinky *\) *)
    | _ -> Curvature.{ angle = Float.pi /. 12.; radius = 85. }

  let col_offsets =
    let space = keyhole.config.outer_w +. spacing in
    let f m i =
      let data = Vec3.(offset_lookup i <+> (space *. Float.of_int i, 0., 0.)) in
      Map.add_exn ~key:i ~data m
    in
    List.fold ~f ~init:(Map.empty (module Int)) (List.range 0 n_cols)

  let centre_offset = Map.find_exn col_offsets centre_col

  let apply_tent off col =
    Column.(rotate_about_pt (0., tent, 0.) Vec3.(off <-> centre_offset) col)

  let place_col ~key:i ~data:off =
    apply_tent off (well_column @@ well_lookup i) |> Column.translate off

  let columns, thumb =
    let placed_cols = Map.mapi ~f:place_col col_offsets in
    let lift =
      let lowest_z =
        let face_low ({ points = ps; _ } : KeyHole.Face.t) =
          KeyHole.Face.Points.fold
            ~f:(fun m p -> Float.min m (Vec3.get_z p))
            ~init:Float.max_value
            ps
        in
        let key_low ({ faces = fs; _ } : _ KeyHole.t) =
          KeyHole.Faces.fold
            ~f:(fun m face -> Float.min m (face_low face))
            ~init:Float.max_value
            fs
        in
        let col_low ({ keys = ks; _ } : _ Column.t) =
          Map.fold
            ~f:(fun ~key:_ ~data m -> Float.min m (key_low data))
            ~init:Float.max_value
            ks
        in
        Map.fold
          ~f:(fun ~key:_ ~data m -> Float.min m (col_low data))
          ~init:Float.max_value
          placed_cols
      in
      clearance -. lowest_z
    in
    let thumb =
      let placed = Column.(rotate thumb_angle thumb |> translate thumb_offset) in
      apply_tent (Map.find_exn placed.keys 1).origin placed
      |> Column.translate (0., 0., lift)
    in
    Map.map ~f:(Column.translate (0., 0., lift)) placed_cols, thumb

  let scad =
    Model.union
      (Map.fold ~f:(fun ~key:_ ~data l -> data.scad :: l) ~init:[ thumb.scad ] columns)

  let wall_start side (k : _ KeyHole.t) =
    let cyl =
      Model.cylinder ~center:true (k.config.thickness /. 2.) k.config.outer_w
      |> Model.rotate (0., Float.pi /. 2., 0.)
    and face = KeyHole.Faces.face k.faces side in
    let r = RotMatrix.(align_exn (KeyHole.Face.direction face) (1., 0., 0.) |> to_euler)
    and t = Vec3.(KeyHole.orthogonal k side <*> (2., 2., 2.)) in
    let centre = Vec3.(face.points.centre <+> t) in
    Model.rotate r cyl |> Model.translate centre, centre

  let bez_wall side i =
    let key, face, y_sign =
      let c = Map.find_exn columns i in
      match side with
      | `North ->
        let key = snd @@ Map.max_elt_exn c.keys in
        key, key.faces.north, 1.
      | `South ->
        let key = Map.find_exn c.keys 0 in
        key, key.faces.south, -1.
    in
    let x_tent, y_tent, z_rot = KeyHole.angles key
    and KeyHole.Face.{ points = { centre = (_, _, cz) as centre; _ }; _ } = face in
    let jog_y = Float.cos x_tent *. (keyhole.config.thickness *. 1.5)
    and jog_x =
      let edge_y = Vec3.get_y face.points.centre in
      match Map.find columns (i + 1) with
      | Some next_c ->
        let right_x = Vec3.get_x face.points.top_right
        and next_key = snd @@ Map.max_elt_exn next_c.keys in
        let diff =
          match side with
          | `North when Float.(Vec3.get_y next_key.faces.north.points.centre >= edge_y) ->
            right_x -. Vec3.get_x next_key.faces.north.points.bot_left
          | `South when Float.(Vec3.get_y next_key.faces.south.points.centre <= edge_y) ->
            right_x -. Vec3.get_x next_key.faces.south.points.bot_left
          | _ -> -.spacing
        in
        if Float.(diff > 0.) then diff +. spacing else Float.max 0. (spacing +. diff)
      | _           -> 0.
    and start =
      (* move out of the key wall, parallel to the keys columnar tilt *)
      Vec3.(
        centre
        <+> ( 0.
            , ((Float.cos x_tent *. keyhole.config.thickness /. 2.) -. 0.001) *. y_sign
            , Float.sin x_tent *. keyhole.config.thickness /. 2. *. y_sign ))
    and chunk =
      Model.cylinder ~center:true (keyhole.config.thickness /. 2.) keyhole.config.outer_w
      |> Model.rotate (0., (Float.pi /. 2.) -. y_tent, z_rot)
    in
    let wall =
      Bezier.quad_hull
        ~t1:(0., 0., 0.)
        ~t2:
          ( -.jog_x
          , y_sign *. (3. +. jog_y)
          , Float.sin x_tent *. (keyhole.config.thickness +. 2.) )
        ~t3:(-.jog_x, y_sign *. (5. +. jog_y), -.cz +. (keyhole.config.thickness /. 2.))
        ~r1:(0., 0., 0.)
        ~r2:(0., y_tent, 0.)
        ~r3:(0., y_tent, 0.)
        ~step:0.1
        chunk
    in
    Model.union
      [ Model.hull [ Model.translate start chunk; face.scad ]
      ; wall |> Model.translate start
      ]

  let get_comp q ax =
    let qx, qy, qz, qw = q in
    let rot_ax = qx, qy, qz in
    let d = Vec3.dot ax rot_ax in
    let proj = Vec3.map (( *. ) d) ax in
    let twist = Quaternion.make proj qw in
    if Float.(d < 0.) then Quaternion.negate twist else twist

  let side_wall ?(z_up = 2.) side d1 d2 (key : _ KeyHole.t) =
    (* let side_point ax z =
     *   match side with
     *   | `North | `South -> 0., ax, z
     *   | `West | `East   -> ax, 0., z
     * in *)
    let chunk, start = wall_start side key
    and chunk_rad = keyhole.config.thickness /. 2.
    and ortho = KeyHole.orthogonal key side in
    let z_hop = (Float.max 0. (Vec3.get_z ortho) *. keyhole.config.thickness) +. z_up in
    let t2 =
      Vec3.(mul (normalize (mul ortho (1., 1., 0.))) (d1, d1, 0.) |> add (0., 0., z_hop))
    and t3 =
      Vec3.(
        sub
          (mul (normalize (mul ortho (1., 1., 0.))) (d2, d2, 0.))
          (0., 0., get_z start -. chunk_rad))
    in
    let face = KeyHole.Faces.face key.faces side in
    (* TODO: Is it possible to prevent the z-rotation that my xy axis rotation is
     * creating, so that the z angle at the bottom matches what it started at?
     * https://stackoverflow.com/questions/3684269/component-of-a-quaternion-rotation-around-an-axis
     * Could I do swing-twist decomposition to get the angle around the z axis, then remove it?
     * Also, is there any better way using plain quaternion composition to cancel out any
     * z rotation before it happens? *)
    let rotator =
      (* let q1 = Quaternion.make (KeyHole.Face.direction face) 0. in *)
      let q1 = Quaternion.id in
      let x, y, z = KeyHole.Face.direction face in
      (* let q1 = RotMatrix.align_exn (0., 0., 1.) (x, y, z) |> Quaternion.of_rotmatrix in *)
      (* let q2 = Quaternion.make (x, y, 0.) (Float.pi /. 2.) in *)
      (* NOTE: This method using align with conj of the rotmatrix projected onto the
       * xy plane (z cancelled) and the face direction negated works for some side walls,
       * but not others. For some it a zero/same vector exception is thrown and for others,
       * the resulting rotaion seems to be 90deg off. Are there special cases that I am missing? *)
      let q2 =
        try
          if Float.(abs y - 0.01 < 0.)
          then
            RotMatrix.align_exn (-.x, -.y, -.z) (x, y, 0.)
            |> Quaternion.of_rotmatrix
            |> Quaternion.conj
          else
            (* RotMatrix.align_exn (x, y, z) (x, y, 0.) |> Quaternion.of_rotmatrix *)
            Quaternion.make (x, y, 0.) (Float.pi /. 2.)
        with
        | _ -> Quaternion.id
      in
      (* Stdio.print_endline (Quaternion.to_string q2); *)
      (* Stdio.print_endline (Quaternion.to_string @@ get_comp q2 (0., 0., 1.)); *)
      (* let q2 = Quaternion.mul (get_comp q2 (0., 0., 1.) |> Quaternion.conj) q2 in *)
      (* Stdio.print_endline (Quaternion.to_string q2); *)
      Bezier.Rotator.make ~pivot:(Vec3.negate start) q1 q2
    in
    (* let cyl =
     *   Model.cylinder ~center:true (key.config.thickness /. 2.) key.config.outer_w
     *   |> Model.rotate (0., Float.pi /. 2., 0.)
     *   |> Model.translate start
     * in *)
    let wall = Bezier.quad_hull' ~rotator ~t1:(0., 0., 0.) ~t2 ~t3 ~step:0.1 chunk in
    Model.union [ Model.hull [ chunk; face.scad ]; wall ]

  let poly_wall ?(z_up = 2.) side d1 d2 (key : _ KeyHole.t) =
    let ortho = KeyHole.orthogonal key side in
    let z_hop = (Float.max 0. (Vec3.get_z ortho) *. keyhole.config.thickness) +. z_up in
    let get_ps top (x, y, z) =
      (* TODO: actual calculation will determine where the points on the ground should
       * be, then calculate the control points based on that (to make sure that the
       * correct wall thickness is achieved).
       *
       * Doing it that way should also fix the problem where the bottom actually juts
       * out more than the top (column walls pointing upwards). *)
      let jog = if top then 3. else 0. in
      let p2 =
        Vec3.(
          mul (normalize (mul ortho (1., 1., 0.))) (d1 +. jog, d1 +. jog, 0.)
          |> add (x, y, z +. z_hop))
      and p3 =
        Vec3.(
          add
            (mul (normalize (mul ortho (1., 1., 0.))) (d2 +. jog, d2 +. jog, 0.))
            (x, y, 0.))
      in
      p2, p3
    in
    let face = KeyHole.Faces.face key.faces side in
    let step = 0.1 in
    let top_left_bez =
      let p2, p3 = get_ps true face.points.top_left in
      Bezier.curve (Bezier.quad_vec3 ~p1:face.points.top_left ~p2 ~p3) step
    in
    let top_right_bez =
      let p2, p3 = get_ps true face.points.top_right in
      Bezier.curve (Bezier.quad_vec3 ~p1:face.points.top_right ~p2 ~p3) step
    in
    let bot_left_bez =
      let p2, p3 = get_ps false face.points.bot_left in
      Bezier.curve (Bezier.quad_vec3 ~p1:face.points.bot_left ~p2 ~p3) step
    in
    let bot_right_bez =
      let p2, p3 = get_ps false face.points.bot_right in
      Bezier.curve (Bezier.quad_vec3 ~p1:face.points.bot_right ~p2 ~p3) step
    in
    (* TODO: avoid concatenating like this if I can, build with recursive function
     * that makes the beziers perhaps *)
    let pts = top_left_bez @ top_right_bez @ bot_right_bez @ bot_left_bez in
    let n = List.length top_left_bez in
    (* TODO: calculate instead of using length *)
    let e = n - 1 in
    let top_left_start = 0 in
    let top_right_start = n in
    let bot_right_start = n * 2 in
    let bot_left_start = n * 3 in
    Model.polyhedron
      pts
      [ [ top_right_start; top_left_start; bot_left_start; bot_right_start ]
      ; [ top_left_start + e
        ; top_right_start + e
        ; bot_right_start + e
        ; bot_left_start + e
        ]
      ; List.range ~stride:(-1) (bot_left_start + e) (bot_left_start - 1)
        @ List.range top_left_start (top_left_start + n)
      ; List.range ~stride:(-1) (top_right_start + e) (top_right_start - 1)
        @ List.range bot_right_start (bot_right_start + n)
      ; List.range ~stride:(-1) (top_left_start + e) (top_left_start - 1)
        @ List.range top_right_start (top_right_start + n)
      ; List.range ~stride:(-1) (bot_right_start + e) (bot_right_start - 1)
        @ List.range bot_left_start (bot_left_start + n)
      ]

  let scad =
    Model.union
      [ scad
        (* ; bez_wall `South 2
         * ; bez_wall `South 3
         * ; bez_wall `South 4
         * ; bez_wall `North 0
         * ; bez_wall `North 1
         * ; bez_wall `North 2
         * ; bez_wall `North 3 (\* ; bez_wall `North 4 *\) *)
        (* ; side_wall ~z_up:2. `North 4. 7. (Map.find_exn (Map.find_exn columns 4).keys 2)
         * ; side_wall ~z_up:2. `South 4. 7. (Map.find_exn (Map.find_exn columns 4).keys 0)
         * ; side_wall ~z_up:2. `West 4. 7. (Map.find_exn (Map.find_exn columns 0).keys 0)
         * ; side_wall ~z_up:2. `West 4. 7. (Map.find_exn (Map.find_exn columns 0).keys 1)
         * ; side_wall `West 4. 7. (Map.find_exn (Map.find_exn columns 0).keys 2)
         * ; side_wall `East 3. 5. (Map.find_exn (Map.find_exn columns 4).keys 2)
         * ; side_wall `North 4. 7. (Map.find_exn thumb.keys 0)
         * ; side_wall `West 4. 7. (Map.find_exn thumb.keys 0)
         * ; side_wall `South 4. 7. (Map.find_exn thumb.keys 0)
         * ; side_wall `South 4. 7. (Map.find_exn thumb.keys 2) *)
      ; poly_wall `South 4. 7. (Map.find_exn thumb.keys 2)
      ; poly_wall `South 4. 7. (Map.find_exn (Map.find_exn columns 4).keys 0)
      ]

  let t = { scad; columns; thumb }
end

let niz_sensor = Sensor.(make Config.a3144)

let niz_platform =
  (* NOTE: BKE ~= 0.95mm; DES ~= 0.73mm. A value of 1.15 seems to fit both without
   * being too tight or loose on either. *)
  Niz.Platform.(
    make
      { w = 20.
      ; dome_w = 19.
      ; dome_waist = 15. (* width at narrow point, ensure enough space at centre *)
      ; dome_thickness = 1.15
      ; base_thickness = 2.25
      ; sensor_depth = 1.5
      ; snap_clearance = 0.3
      ; snap_len = 0.7
      ; lug_height = 1.5
      ; sensor_config = Sensor.Config.a3144_print
      })

let niz_cross_section =
  Model.difference
    (Model.union
       [ Model.translate
           (0., 0., niz_platform.wall_height +. (keyhole.config.thickness /. 2.))
           keyhole.scad
       ; niz_platform.scad
       ] )
    [ Model.cube ~center:true (25., 15., 20.) |> Model.translate (0., -7.5, 0.) ]
