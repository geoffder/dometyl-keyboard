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
    ~curve:Curvature.(place ~well:{ angle = Math.pi /. 12.; radius = 85. } ~centre_idx)
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
            ~well:{ angle = Math.pi /. 12.; radius = 85. }
            ~fan:{ angle = Math.pi /. 12.; radius = 85. }
            ~centre_idx:1)
      (KeyHole.rotate (0., 0., Math.pi /. 2.) keyhole)
    (* orient along x-axis *)
    |> rotate (0., 0., Math.pi /. -2.))

module Plate = struct
  type 'k t =
    { scad : Model.t
    ; columns : 'k Column.t Map.M(Int).t
    ; thumb : 'k Column.t
    }

  let n_cols = 5
  let spacing = 2.
  let centre_col = 2
  let tent = Math.pi /. 6.
  let thumb_offset = 7., -50., -3.
  let thumb_angle = Math.(0., pi /. -4., pi /. 5.)
  let clearance = 7.

  (* TODO: tune *)
  let offset_lookup = function
    | 2 -> 0., 6., -6. (* middle *)
    | 3 -> 0., 3., -2. (* ring *)
    | i when i >= 4 -> 0., -12., 6. (* pinky *)
    | _ -> 0., 0., 0.

  let well_lookup = function
    (* | i when i >= 4 -> Curvature.{ angle = Math.pi /. 9.; radius = 60. } (\* pinky *\) *)
    | _ -> Curvature.{ angle = Math.pi /. 12.; radius = 85. }

  let col_offsets =
    let space = keyhole.config.outer_w +. spacing in
    let f m i =
      let data = Util.(offset_lookup i <+> (space *. Float.of_int i, 0., 0.)) in
      Map.add_exn ~key:i ~data m
    in
    List.fold ~f ~init:(Map.empty (module Int)) (List.range 0 n_cols)

  let centre_offset = Map.find_exn col_offsets centre_col

  let apply_tent off col =
    Column.(rotate_about_pt (0., tent, 0.) Util.(off <-> centre_offset) col)

  let place_col ~key:i ~data:off =
    apply_tent off (well_column @@ well_lookup i) |> Column.translate off

  let columns, thumb =
    let placed_cols = Map.mapi ~f:place_col col_offsets in
    let lift =
      let lowest_z =
        let face_low ({ points = ps; _ } : KeyHole.Face.t) =
          KeyHole.Face.Points.fold
            ~f:(fun m p -> Float.min m (Util.get_z p))
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
      let edge_y = Util.get_y face.points.centre in
      match Map.find columns (i + 1) with
      | Some next_c ->
        let right_x = Util.get_x face.points.top_right
        and next_key = snd @@ Map.max_elt_exn next_c.keys in
        let diff =
          match side with
          | `North when Float.(Util.get_y next_key.faces.north.points.centre >= edge_y) ->
            right_x -. Util.get_x next_key.faces.north.points.bot_left
          | `South when Float.(Util.get_y next_key.faces.south.points.centre <= edge_y) ->
            right_x -. Util.get_x next_key.faces.south.points.bot_left
          | _ -> -.spacing
        in
        if Float.(diff > 0.) then diff +. spacing else Float.max 0. (spacing +. diff)
      | _           -> 0.
    and start =
      (* move out of the key wall, parallel to the keys columnar tilt *)
      Util.(
        centre
        <+> ( 0.
            , ((Float.cos x_tent *. keyhole.config.thickness /. 2.) -. 0.001) *. y_sign
            , Float.sin x_tent *. keyhole.config.thickness /. 2. *. y_sign ))
    and chunk =
      Model.cylinder ~center:true (keyhole.config.thickness /. 2.) keyhole.config.outer_w
      |> Model.rotate (0., (Math.pi /. 2.) -. y_tent, z_rot)
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

  (* TODO:
   * - this is working for North-South and East-West axis walls, which is good,
   * and has a large amount of overlap with the bez_wall function, though this one
   * lacks knowledge of the columns (since it just takes a key). May be able to calculate
   * the x jogging for the N-S column walls and give that as an argument to this one.
   * - the issue which I have to think about/resolve next is that the thumb cluster breaks the
   * assumption that N-S of a key is niceley along Y and E-W along X. I thought I resolved
   * this based on my simple splay test, but it seems like I wasn't really looking close enough.
   * Much like how I calculate the distance to jog along the plane of the key, I need to
   * to the same for the walls jut out for the bezier curve. Otherwise the walls do not
   * flow naturally from the keys when they are at very rotated angles as they are in the thumb.
   * - there is a slight issue with E-W walls not seeming to not make contact with the
   * the keykole edge. Problem is not present on N-S walls. I think it has to do with the
   * bowing of the radius. Need to figure out the correct angle calculations to correct it
   * (likely will be an issue for the thumb walls as well) *)
  let side_wall side d1 d2 (key : _ KeyHole.t) =
    let face, sign =
      match side with
      | `North -> key.faces.north, 1.
      | `South -> key.faces.south, -1.
      | `West  -> key.faces.west, -1.
      | `East  -> key.faces.east, 1.
    in
    let side_point ax z =
      match side with
      | `North | `South -> 0., ax, z
      | `West | `East   -> ax, 0., z
    in
    let x_tent, y_tent, z_rot = KeyHole.angles key in
    (* let x_tent, y_tent, z_rot = KeyHole.Face.angles face in *)
    (* KeyHole.Face.angles face |> ignore; *)
    let ax_angle =
      match side with
      | `North | `South ->
        (function
        | `Trans -> x_tent
        | `Rot   -> -.y_tent )
      | `West | `East   ->
        (function
        | `Trans -> y_tent
        | `Rot   -> x_tent )
    and KeyHole.Face.{ points = { centre = (_, _, cz) as centre; _ }; _ } = face in
    let jog =
      (* Don't jog out further if already pointed downward (nothing to avoid). *)
      let a = ax_angle `Trans in
      match side with
      | (`East | `North) when Float.(a < 0.) -> 0.
      | (`South | `West) when Float.(a > 0.) -> 0.
      | _ -> Float.cos a *. (keyhole.config.thickness *. 1.5) *. sign
    and start =
      (* move out of the key wall, parallel to the keys columnar tilt *)
      let t =
        match side with
        | `North | `South ->
          ( 0.
          , ((Float.cos x_tent *. keyhole.config.thickness /. 2.) -. 0.001) *. sign
          , Float.sin x_tent *. keyhole.config.thickness /. 2. *. sign )
        | `West | `East   ->
          ( ((Float.cos y_tent *. keyhole.config.thickness /. 2.) -. 0.001) *. sign
          , 0.
          , Float.sin y_tent *. keyhole.config.thickness /. 2. *. sign )
      in
      Util.(centre <+> t)
    and chunk =
      (* NOTE: this is to test what would solve the lack of alignment of the first
       * chunk with the face of the keyhole in east/west. This value of pi/24 seems
       * bang on for the southern key hole (with the current radius and angle of the
       * columns). Although the z-rotation comes up as zero when measuring the x-y
       * axis angle of the "horizontal" faces of the keyholes, the fact that the
       * columns are "bent" means that when tented the rotations required to match each
       * key are slightly different. Can I use trig to calculate the appropriate z-rotation
       * to remedy this? Or should I try calculating more angles to describe the keyhole? *)
      (* let z_rot =
       *   match side with
       *   | `North | `South -> z_rot
       *   | _               -> Math.pi /. 24.
       * in *)
      (* Stdio.printf "zrot: %.2f \n" z_rot; *)
      Model.cylinder ~center:true (keyhole.config.thickness /. 2.) keyhole.config.outer_w
      |> Model.rotate
           Util.(side_point (ax_angle `Rot) z_rot <+> side_point (Math.pi /. 2.) 0.)
    in
    let wall =
      Bezier.quad_hull
        ~t1:(0., 0., 0.)
        ~t2:
          (side_point
             ((sign *. d1) +. jog)
             (Float.sin (ax_angle `Trans) *. (keyhole.config.thickness +. 2.)) )
        ~t3:(side_point ((sign *. d2) +. jog) (-.cz +. (keyhole.config.thickness /. 2.)))
        ~r1:(0., 0., 0.)
        ~r2:(side_point (-.ax_angle `Rot) 0.)
        ~r3:(side_point (-.ax_angle `Rot) 0.)
        ~step:0.1
        chunk
    in
    wall |> Model.translate start
  (* Model.union
   *   [ Model.hull [ Model.translate start chunk; face.scad ]
   *   ; wall |> Model.translate start
   *   ] *)

  let block = Model.cylinder ~center:true 2. 15. |> Model.rotate (0., Math.pi /. 2., 0.)

  (* let block = Model.cylinder ~center:true 2. 15. |> Model.rotate (Math.pi /. 2., 0., 0.) *)
  (* let block = Model.cylinder ~center:true 2. 15. *)

  type mat =
    { x : Core.pos_t
    ; y : Core.pos_t
    ; z : Core.pos_t
    }

  let rot_matrix a b =
    if false (* check if they are equal, also should deal with 0 vecs and X = -Y *)
    then None
    else (
      let x = Math.norm a
      and z = Math.(norm (cross a b)) in
      Some { x; y = Math.(norm (cross z x)); z } )

  let rot_matrix_exn a b = Option.value_exn (rot_matrix a b)

  let euler_of_matrix m =
    let open Util in
    let theta = -1. *. Float.sin (get_z m.x) in
    let cos_theta = Float.cos theta in
    let psi = Float.atan2 (get_z m.y /. cos_theta) (get_z m.z /. cos_theta) in
    let phi = Float.atan2 (get_y m.x /. cos_theta) (get_x m.x /. cos_theta) in
    psi, theta, phi

  let euler_of_matrix' m =
    let open Util in
    let x = Float.atan2 (get_z m.y) (get_z m.z) in
    let y =
      Float.atan2
        (-1. *. get_z m.x)
        (Float.sqrt ((get_z m.y *. get_z m.y) +. (get_z m.z *. get_z m.z)))
    in
    let z = Float.atan2 (get_y m.x) (get_x m.x) in
    x, y, z

  let euler_block side (k : _ KeyHole.t) =
    let face = KeyHole.Faces.face k.faces side in
    let r =
      rot_matrix_exn (KeyHole.Face.direction face) (1., 0., 0.) |> euler_of_matrix'
    in
    Model.rotate r block |> Model.translate face.points.centre

  let scad =
    Model.union
      [ scad
        (* ; bez_wall `South 2
         * ; bez_wall `South 3
         * ; bez_wall `South 4
         * ; bez_wall `North 0
         * ; bez_wall `North 1
         * ; bez_wall `North 2
         * ; bez_wall `North 3 (\* ; bez_wall `North 4 *\)
         * ; side_wall `North 3. 5. (Map.find_exn (Map.find_exn columns 4).keys 2)
         * ; side_wall `West 3. 5. (Map.find_exn (Map.find_exn columns 0).keys 0)
         * ; side_wall `West 3. 5. (Map.find_exn (Map.find_exn columns 0).keys 1)
         * ; side_wall `West 3. 5. (Map.find_exn (Map.find_exn columns 0).keys 2)
         * ; side_wall `East 1. 3. (Map.find_exn (Map.find_exn columns 4).keys 2)
         * ; side_wall `North 3. 5. (Map.find_exn thumb.keys 0)
         * ; side_wall `West 3. 5. (Map.find_exn thumb.keys 0)
         * ; side_wall `South 3. 5. (Map.find_exn thumb.keys 0)
         * ; side_wall `South 3. 5. (Map.find_exn thumb.keys 2) *)
      ; block
      ; euler_block `South (Map.find_exn thumb.keys 1)
      ; euler_block `South (Map.find_exn thumb.keys 2)
      ; euler_block `West (Map.find_exn (Map.find_exn columns 0).keys 2)
      ; euler_block `West (Map.find_exn (Map.find_exn columns 0).keys 1)
      ; euler_block `West (Map.find_exn (Map.find_exn columns 0).keys 0)
      ; euler_block `South (Map.find_exn (Map.find_exn columns 0).keys 0)
      ; euler_block `North (Map.find_exn (Map.find_exn columns 0).keys 2)
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
