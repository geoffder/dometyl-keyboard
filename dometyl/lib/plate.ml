open Base
open Scad_ml

module Lookups = struct
  type t =
    { offset : int -> Vec3.t
    ; well : int -> Curvature.spec
    ; splay : int -> float
    }

  let default_offset = function
    | 2 -> 0., 9., -6. (* middle *)
    | 3 -> 0., 5., -2. (* ring *)
    (* | i when i >= 4 -> 0., -12., 6. (\* pinky *\) *)
    | i when i >= 4 -> 1.5, -21., 6. (* pinky *)
    (* | 0 -> 0., 0., 6.5  (* first test with *) *)
    | 0 -> -1., 0., 5.5
    | _ -> 0., 0., 0.

  let default_well = function
    | i when i >= 4 ->
      Curvature.{ angle = Float.pi /. 8.; radius = 58.; tilt = 0. } (* pinky *)
    (* | i when i = 0 ->
       (* first print with *)
     *   Curvature.{ angle = Float.pi /. 8.; radius = 58.; tilt = Float.pi /. 7.5 } *)
    | i when i = 0 ->
      Curvature.{ angle = Float.pi /. 8.; radius = 58.; tilt = Float.pi /. 6.75 }
    | _ -> Curvature.{ angle = Float.pi /. 8.; radius = 60.; tilt = 0. }
  (* | _ -> Curvature.{ angle = Float.pi /. 12.; radius = 85. } *)

  let default_splay = function
    | i when i >= 4 -> Float.pi /. -20. (* pinky *)
    | _ -> 0.

  let make ?(offset = default_offset) ?(well = default_well) ?(splay = default_splay) () =
    { offset; well; splay }
end

type 'k config =
  { n_rows : int
  ; centre_row : int
  ; n_cols : int
  ; centre_col : int
  ; spacing : float
  ; tent : float
  ; clearance : float
  ; thumb_offset : Vec3.t
  ; thumb_angle : Vec3.t
  }

type 'k t =
  { config : 'k config
  ; scad : Model.t
  ; columns : 'k Column.t Map.M(Int).t
  ; thumb : 'k Column.t
  }

let make_thumb ?well ?fan keyhole =
  Column.(
    make
      ~join_ax:`EW
      ~n_keys:3
      ~curve:Curvature.(place ?well ?fan ~centre_idx:1)
      (KeyHole.rotate (0., 0., Float.pi /. 2.) keyhole)
    (* orient along x-axis *)
    |> rotate (0., 0., Float.pi /. -2.))

(* Old
   ?(thumb_angle = Float.(0., pi /. -4., pi /. 5.))
   ?(thumb_fan = Curvature.{ angle = Float.pi /. 10.5; radius = 85.; tilt = 0. })
   ?(thumb_well = Curvature.{ angle = Float.pi /. 5.; radius = 50.; tilt = 0. })
*)
let make
    ?(n_rows = 3)
    ?(centre_row = 1)
    ?(n_cols = 5)
    ?(centre_col = 2)
    ?(spacing = 2.)
    ?(clearance = 6.5)
    ?(tent = Float.pi /. 12.)
    ?(thumb_offset = 7., -50., -3.)
    ?(thumb_angle = Float.(0., pi /. -5.5, pi /. 5.))
    ?(thumb_fan = Curvature.{ angle = Float.pi /. 8.25; radius = 70.; tilt = 0. })
    ?(thumb_well = Curvature.{ angle = Float.pi /. 6.; radius = 70.; tilt = 0. })
    ?(lookups = Lookups.make ())
    (keyhole : _ KeyHole.t)
  =
  let well_column spec =
    Column.make
      ~n_keys:n_rows
      ~curve:Curvature.(place ~well:spec ~centre_idx:centre_row)
      keyhole
  in
  let col_offsets =
    let space = keyhole.config.outer_w +. spacing in
    let f m i =
      let data = Vec3.(lookups.offset i <+> (space *. Float.of_int i, 0., 0.)) in
      Map.add_exn ~key:i ~data m
    in
    List.fold ~f ~init:(Map.empty (module Int)) (List.range 0 n_cols)
  in
  let centre_offset = Map.find_exn col_offsets centre_col in
  let apply_tent off col =
    Column.(rotate_about_pt (0., tent, 0.) Vec3.(off <-> centre_offset) col)
  in
  let place_col ~key:i ~data:off =
    apply_tent off (well_column @@ lookups.well i)
    |> Column.rotate (0., 0., lookups.splay i)
    |> Column.translate off
  in
  let columns, thumb =
    let placed_cols = Map.mapi ~f:place_col col_offsets in
    let lift =
      let lowest_z =
        let face_low ({ points = ps; _ } : KeyHole.Face.t) =
          Points.fold ~f:(fun m p -> Float.min m (Vec3.get_z p)) ~init:Float.max_value ps
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
      Column.translate (0., 0., clearance -. lowest_z)
    in
    let thumb =
      let placed =
        Column.(
          make_thumb ~fan:thumb_fan ~well:thumb_well keyhole
          |> rotate thumb_angle
          |> translate thumb_offset)
      in
      apply_tent (Map.find_exn placed.keys 1).origin placed |> lift
    in
    Map.map ~f:lift placed_cols, thumb
  in
  { config =
      { n_rows
      ; centre_row
      ; n_cols
      ; centre_col
      ; spacing
      ; tent
      ; clearance
      ; thumb_offset
      ; thumb_angle
      }
  ; scad =
      Model.union
        (Map.fold ~f:(fun ~key:_ ~data l -> data.scad :: l) ~init:[ thumb.scad ] columns)
  ; columns
  ; thumb
  }

let column_joins { config = { n_cols; _ }; columns; _ } =
  let join = Bridge.cols ~columns in
  Model.union (List.init ~f:(fun i -> join i (i + 1)) (n_cols - 1))

(* TODO: testing *)
let skeleton_bridges { config = { n_rows; n_cols; _ }; columns; _ } =
  let bridge c k =
    Bridge.keys (Columns.key_exn columns c k) (Columns.key_exn columns (c + 1) k)
  in
  Model.union
  @@ List.init
       ~f:(fun i -> if i < 2 then bridge i 0 else bridge i (n_rows - 1))
       (n_cols - 1)

let to_scad t = t.scad
