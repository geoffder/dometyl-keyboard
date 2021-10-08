open! Base
open! Scad_ml
open Infix

type bump_loc =
  | Thumb of Util.idx
  | Col of Util.idx * Util.idx
  | Point of Vec3.t

let locate_bump (plate : _ Plate.t) = function
  | Thumb k    ->
    let%map.Option key = Util.idx_to_find k plate.thumb.keys in
    Vec3.mul (1., 1., 0.) key.origin
  | Col (c, k) ->
    let%bind.Option col = Util.idx_to_find c plate.columns in
    let%map.Option key = Util.idx_to_find k col.keys in
    Vec3.mul (1., 1., 0.) key.origin
  | Point p    -> Some p

let default_bumps =
  [ Thumb First
  ; Col (First, Last)
  ; Col (Idx 3, Last)
  ; Col (Last, Last)
  ; Col (Last, First)
  ]

let make
    ?(thickness = 1.65)
    ?fastener
    ?(bumpon_rad = 5.5)
    ?(bumpon_inset = 0.5)
    ?(bump_locs = default_bumps)
    case
  =
  let screws = Walls.collect_screws case.Case.walls in
  let screw_config = (List.hd_exn screws).config in
  let thickness, screw_hole =
    let fastener =
      match fastener with
      | None          ->
        ( match screw_config with
        | { hole = Through; _ } -> Eyelet.screw_fastener ()
        | _                     -> Magnet )
      | Some fastener -> fastener
    in
    match fastener with
    | Screw { head_rad; shaft_rad; sink = Counter; _ } ->
      let s = shaft_rad /. head_rad in
      ( thickness
      , Scad.circle head_rad |> Scad.linear_extrude ~height:thickness ~scale:(s, s) )
    | Screw { head_rad; shaft_rad; sink = Pan depth; _ } ->
      ( thickness
      , Scad.union
          [ Scad.cylinder ~fn:32 head_rad depth
          ; Scad.cylinder ~fn:32 shaft_rad thickness
          ] )
    | Magnet ->
      let Eyelet.{ inner_rad; thickness = thick; hole; _ } = screw_config in
      let h =
        match hole with
        | Eyelet.Inset inset -> inset
        | _                  -> 0.
      and thickness = Float.max thickness thick in
      ( thickness
      , Scad.union
          [ Scad.translate (0., 0., thickness -. h) @@ Scad.cylinder ~fn:32 inner_rad h
          ; Scad.cylinder ~fn:32 (inner_rad /. 2.) (thickness -. h)
          ] )
  and insets =
    let cut = Scad.cylinder bumpon_rad bumpon_inset in
    List.filter_map
      ~f:(locate_bump case.plate >> Option.map ~f:(Fn.flip Scad.translate cut))
      bump_locs
    |> Scad.union
  in
  let plate =
    Scad.polygon (Connect.outline_2d case.connections)
    |> Scad.linear_extrude ~height:thickness
  in
  Scad.difference
    plate
    (insets :: List.map ~f:(fun l -> Scad.translate l.centre screw_hole) screws)
