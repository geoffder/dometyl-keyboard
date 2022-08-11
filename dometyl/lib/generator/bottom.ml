open! Base
open! Scad_ml
open Infix

type bump_loc =
  | Thumb of Util.idx * Util.idx
  | Body of Util.idx * Util.idx
  | Point of V3.t

let locate_bump (plate : _ Plate.t) = function
  | Thumb (c, k) ->
    let%bind.Option col = Util.idx_to_find c plate.thumb in
    let%map.Option key = Util.idx_to_find k col.keys in
    V3.mul (v3 1. 1. 0.) key.origin
  | Body (c, k)  ->
    let%bind.Option col = Util.idx_to_find c plate.body in
    let%map.Option key = Util.idx_to_find k col.keys in
    V3.mul (v3 1. 1. 0.) key.origin
  | Point p      -> Some p

let default_bumps =
  [ Thumb (Last, First)
  ; Thumb (Last, Last)
  ; Body (First, Last)
  ; Body (Idx 3, Last)
  ; Body (Last, Last)
  ; Body (Last, First)
  ]

let make
    ?(thickness = 1.65)
    ?fastener
    ?(bumpon_rad = 5.5)
    ?(bumpon_inset = 0.8)
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
        | _                     -> SameMagnet )
      | Some fastener -> fastener
    and magnetize rad h =
      let thickness = Float.max thickness (h +. 0.6) in
      ( thickness
      , Scad.union
          [ Scad.translate (v3 0. 0. (thickness -. h))
            @@ Scad.cylinder ~fn:32 ~height:h rad
          ; Scad.cylinder ~fn:32 ~height:(thickness -. h) (rad /. 2.)
          ] )
    in
    match fastener with
    | Screw { head_rad; shaft_rad; sink = Counter; _ } ->
      let s = shaft_rad /. head_rad in
      ( thickness
      , Scad.circle head_rad |> Scad.linear_extrude ~height:thickness ~scale:(v2 s s) )
    | Screw { head_rad; shaft_rad; sink = Pan depth; _ } ->
      ( thickness
      , Scad.union
          [ Scad.cylinder ~fn:32 ~height:depth head_rad
          ; Scad.cylinder ~fn:32 ~height:thickness shaft_rad
          ] )
    | SameMagnet ->
      let Eyelet.{ inner_rad; hole; _ } = screw_config in
      let h =
        match hole with
        | Eyelet.Inset inset -> inset
        | _                  -> failwith "Case eyelet expected to be magnet inset."
      in
      magnetize inner_rad h
    | Magnet { rad; thickness } -> magnetize rad thickness
  and insets =
    let cut = Scad.cylinder ~height:bumpon_inset bumpon_rad in
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
