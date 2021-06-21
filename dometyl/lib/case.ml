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

  let side_wall ?(x_off = 0.) ?(y_off = 0.) ?(z_off = 2.) side d1 d2 (key : _ KeyHole.t) =
    let chunk, (x0, y0, z0) = wall_start side key
    and chunk_rad = keyhole.config.thickness /. 2.
    and ortho = KeyHole.orthogonal key side in
    let z_hop = (Float.max 0. (Vec3.get_z ortho) *. keyhole.config.thickness) +. z_off in
    let t2 =
      Vec3.(
        mul (normalize (mul ortho (1., 1., 0.))) (d1, d1, 0.) |> add (x_off, y_off, z_hop))
    and t3 =
      Vec3.(
        add
          (mul (normalize (mul ortho (1., 1., 0.))) (d2, d2, 0.))
          (x_off, y_off, chunk_rad -. z0 -. (keyhole.config.outer_w /. 2.)))
    in
    let face = KeyHole.Faces.face key.faces side in
    let wall = Bezier.quad_hull' ~t1:(0., 0., 0.) ~t2 ~t3 ~step:0.1 chunk in
    Model.difference
      (Model.union [ Model.hull [ chunk; face.scad ]; wall ])
      [ Model.cube
          ~center:true
          ( keyhole.config.outer_w *. 2.
          , keyhole.config.outer_w *. 2.
          , keyhole.config.outer_w *. 2. )
        |> Model.translate (x0, y0, -.keyhole.config.outer_w)
      ]

  let bez_wall ?z_off side d1 d2 col_idx =
    let key, face, hanging =
      let c = Map.find_exn columns col_idx in
      match side with
      | `North ->
        let key = snd @@ Map.max_elt_exn c.keys in
        let edge_y = Vec3.get_y key.faces.north.points.centre in
        key, key.faces.north, Float.(( <= ) edge_y)
      | `South ->
        let key = Map.find_exn c.keys 0 in
        let edge_y = Vec3.get_y key.faces.south.points.centre in
        key, key.faces.south, Float.(( >= ) edge_y)
    in
    let x_dodge =
      match Map.find columns (col_idx + 1) with
      | Some next_c ->
        let right_x = Vec3.get_x face.points.top_right
        and next_face =
          KeyHole.Faces.face (snd @@ Map.max_elt_exn next_c.keys).faces side
        in
        let diff =
          if hanging (Vec3.get_y next_face.points.centre)
          then right_x -. Vec3.get_x next_face.points.bot_left
          else -.spacing
        in
        if Float.(diff > 0.) then diff +. spacing else Float.max 0. (spacing +. diff)
      | _           -> 0.
    in
    side_wall ~x_off:(x_dodge *. -1.) ?z_off side d1 d2 key

  let poly_wall ?(z_up = 2.) side d1 d2 (key : _ KeyHole.t) =
    (* TODO: using very large bezier steps right now since the varying number of
     * points that it takes to get to the floor for eack side of the key when tented
     * above a certain amount causes problems for the polyhedron creation. Seems like
     * going down in z a similar amount for each side then cutting off below the
     * floor afterwards will be the simplest way to make sure the triangle mesh is
     * able to form well. *)
    let ortho = KeyHole.orthogonal key side in
    let z_hop = (Float.max 0. (Vec3.get_z ortho) *. keyhole.config.thickness) +. z_up in
    let face = KeyHole.Faces.face key.faces side in
    let top_offset =
      Vec3.(mul (1., 1., 0.) (face.points.bot_right <-> face.points.top_right))
    in
    let get_bez top ((x, y, z) as p1) =
      let jog, d1 =
        if top then keyhole.config.thickness, d1 +. ((d2 -. d1) /. 2.) else 0., d1
      in
      let xy = Vec3.(normalize (mul ortho (1., 1., 0.))) in
      let p2 =
        Vec3.(
          mul xy (d1 +. jog, d1 +. jog, 0.)
          |> add (x, y, z +. z_hop)
          |> add (if top then top_offset else 0., 0., 0.))
      and p3 =
        Vec3.(
          add (mul xy (d2 +. jog, d2 +. jog, 0.)) (x, y, 0.)
          |> add (if top then top_offset else 0., 0., 0.))
      in
      Bezier.quad_vec3 ~p1 ~p2 ~p3
    in
    Bezier.prism_exn
      [ get_bez true face.points.top_right
      ; get_bez true face.points.top_left
      ; get_bez false face.points.bot_left
      ; get_bez false face.points.bot_right
      ]
      0.25

  let key_bridge k1 k2 = Model.hull KeyHole.[ k1.faces.east.scad; k2.faces.west.scad ]
  let join_bridge j1 j2 = Model.hull Column.Join.[ j1.faces.east; j2.faces.west ]

  let join_columns a_i b_i =
    let a = Map.find_exn columns a_i
    and b = Map.find_exn columns b_i in
    let folder ~f ~key:_ ~data hulls =
      match data with
      | `Both (a', b') -> f a' b' :: hulls
      | _              -> hulls
    in
    Model.union
      (Map.fold2
         ~f:(folder ~f:key_bridge)
         ~init:(Map.fold2 ~f:(folder ~f:join_bridge) ~init:[] a.joins b.joins)
         a.keys
         b.keys )

  let scad =
    Model.union
      [ scad
        (* ; bez_wall `North 4. 7. 0
         * ; bez_wall `North 4. 7. 1
         * ; bez_wall `North 4. 7. 2
         * ; bez_wall `South 4. 7. 2
         * ; bez_wall `North 4. 7. 3
         * ; bez_wall `South 4. 7. 3
         * ; bez_wall `North 4. 7. 4
         * ; bez_wall `South 4. 7. 4
         * ; side_wall ~z_off:2. `West 4. 7. (Map.find_exn (Map.find_exn columns 0).keys 0)
         * ; side_wall ~z_off:2. `West 4. 7. (Map.find_exn (Map.find_exn columns 0).keys 1) *)
        (* ; side_wall `East 3. 5. (Map.find_exn (Map.find_exn columns 4).keys 2) *)
        (* ; side_wall `North 4. 7. (Map.find_exn thumb.keys 0)
         * ; side_wall `West 4. 7. (Map.find_exn thumb.keys 0)
         * ; side_wall `South 4. 7. (Map.find_exn thumb.keys 0)
         * ; side_wall `South 4. 7. (Map.find_exn thumb.keys 2)
         * ; side_wall `West 4. 7. (Map.find_exn (Map.find_exn columns 0).keys 2) *)
      ; join_columns 0 1
      ; join_columns 1 2
      ; join_columns 2 3
      ; join_columns 3 4
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
