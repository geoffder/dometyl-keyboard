open! Base
open! Scad_ml

type presence =
  | No
  | Yes
  | Eye

module Body = struct
  module Cols = struct
    type col =
      { north : Wall.t option
      ; south : Wall.t option
      }
    [@@deriving scad]

    let map_col ~f c = { north = Option.map ~f c.north; south = Option.map ~f c.south }

    type t = (col Map.M(Int).t[@scad.mapf]) [@@deriving scad]

    let make
        ?(d1 = 2.)
        ?(d2 = 5.)
        ?(z_off = 0.)
        ?(thickness = 3.5)
        ?index_thickness
        ?(north_clearance = 2.5)
        ?(south_clearance = 2.5)
        ?(n_steps = `Flat 4)
        ?(n_facets = 1)
        ?(north_lookup = fun i -> if i = 2 || i = 4 then Eye else Yes)
        ?(south_lookup =
          function
          | i when i = 3 -> Eye
          | i when i > 1 -> Yes
          | _ -> No)
        ?(eyelet_config = Eyelet.m4_config)
        Plate.{ config = { spacing; _ }; columns; _ }
      =
      let drop = Wall.column_drop ~spacing ~columns ~z_off ~n_steps ~n_facets ~d1 ~d2
      and index_thickness' = Option.value ~default:thickness index_thickness in
      let bez_wall north presence =
        let clearance = if north then north_clearance else south_clearance in
        match presence with
        | No  -> fun _ _ -> None
        | Yes ->
          fun side i ->
            Some
              (drop
                 ~clearance
                 ~thickness:(if i < 2 then index_thickness' else thickness)
                 side
                 i )
        | Eye ->
          fun side i ->
            Some
              (drop
                 ~clearance
                 ~thickness:(if i < 2 then index_thickness' else thickness)
                 ~eyelet_config
                 side
                 i )
      in
      Map.mapi
        ~f:(fun ~key:i ~data:_ ->
          { north = bez_wall true (north_lookup i) `North i
          ; south = bez_wall false (south_lookup i) `South i
          } )
        columns

    let get t = function
      | `N -> t.north
      | `S -> t.south

    let col_to_scad col =
      Scad.union
        (List.filter_map ~f:(Option.map ~f:Wall.to_scad) [ col.north; col.south ])

    let to_scad t =
      Scad.union (Map.fold ~init:[] ~f:(fun ~key:_ ~data l -> col_to_scad data :: l) t)

    let collect_screws ?(init = []) (t : t) =
      let prepend wall =
        Util.prepend_opt @@ Option.bind ~f:(fun w -> w.Wall.screw) wall
      in
      Map.fold
        ~init
        ~f:(fun ~key:_ ~data l -> prepend data.north l |> prepend data.south)
        t
  end

  module Sides = struct
    type t =
      { west : Wall.t Map.M(Int).t [@scad.mapf]
      ; east : Wall.t Map.M(Int).t [@scad.mapf]
      }
    [@@deriving scad]

    let map ~f t = { west = Map.map ~f t.west; east = Map.map ~f t.east }

    let make
        ?(d1 = 2.)
        ?(d2 = 5.)
        ?(z_off = 0.)
        ?(thickness = 3.5)
        ?(clearance = 3.)
        ?(n_steps = `Flat 4)
        ?(n_facets = 1)
        ?(west_lookup = fun i -> if i = 0 then Eye else No)
        ?(east_lookup = fun _ -> No)
        ?(eyelet_config = Eyelet.m4_config)
        Plate.{ columns; _ }
      =
      let west_col = Map.find_exn columns 0
      and _, east_col = Map.max_elt_exn columns
      and siding =
        Wall.poly_siding ~d1 ~d2 ~z_off ~thickness ~clearance ~n_steps ~n_facets
      in
      let sider side ~key ~data m =
        let lookup =
          match side with
          | `West -> west_lookup
          | `East -> east_lookup
        in
        match lookup key with
        | Yes -> Map.add_exn ~key ~data:(siding side data) m
        | Eye -> Map.add_exn ~key ~data:(siding ~eyelet_config side data) m
        | No  -> m
      in
      { west = Map.fold ~init:(Map.empty (module Int)) ~f:(sider `West) west_col.keys
      ; east = Map.fold ~init:(Map.empty (module Int)) ~f:(sider `East) east_col.keys
      }

    let to_scad t =
      let f ~key:_ ~data l = Wall.to_scad data :: l in
      Scad.union (Map.fold ~init:(Map.fold ~init:[] ~f t.west) ~f t.east)

    let collect_screws ?(init = []) (t : t) =
      let f ~key:_ ~data l =
        Option.value_map ~default:l ~f:(fun s -> s :: l) data.Wall.screw
      in
      Map.fold ~init:(Map.fold ~init ~f t.west) ~f t.east
  end

  type t =
    { cols : Cols.t
    ; sides : Sides.t
    }
  [@@deriving scad]

  (* TODO: rough draft. This impl does not allow for different settings between cols
   * and siding. Is that fine? Or should I add more params here, or just make separately? *)
  let make
      ?d1
      ?d2
      ?z_off
      ?thickness
      ?index_thickness
      ?north_clearance
      ?south_clearance
      ?side_clearance
      ?n_steps
      ?n_facets
      ?north_lookup
      ?south_lookup
      ?west_lookup
      ?east_lookup
      ?eyelet_config
      plate
    =
    { cols =
        Cols.make
          ?d1
          ?d2
          ?z_off
          ?thickness
          ?index_thickness
          ?north_clearance
          ?south_clearance
          ?n_steps
          ?n_facets
          ?north_lookup
          ?south_lookup
          ?eyelet_config
          plate
    ; sides =
        Sides.make
          ?d1
          ?d2
          ?z_off
          ?thickness
          ?clearance:side_clearance
          ?n_steps
          ?n_facets
          ?west_lookup
          ?east_lookup
          ?eyelet_config
          plate
    }

  let to_scad t = Scad.union [ Cols.to_scad t.cols; Sides.to_scad t.sides ]

  let collect_screws ?(init = []) (t : t) =
    Cols.collect_screws ~init:(Sides.collect_screws ~init t.sides) t.cols
end

module Thumb = struct
  type key =
    { north : Wall.t option
    ; south : Wall.t option
    }
  [@@deriving scad]

  let map_key ~f k = { north = Option.map ~f k.north; south = Option.map ~f k.south }

  type sides =
    { west : Wall.t option
    ; east : Wall.t option
    }
  [@@deriving scad]

  let map_sides ~f s = { west = Option.map ~f s.west; east = Option.map ~f s.east }

  let get_side sides = function
    | `W -> sides.west
    | `E -> sides.east

  type t =
    { keys : key Map.M(Int).t [@scad.mapf]
    ; sides : sides
    }
  [@@deriving scad]

  let map ~f t = { keys = Map.map ~f:(map_key ~f) t.keys; sides = map_sides ~f t.sides }

  let make
      ?(d1 = 1.)
      ?(d2 = 3.)
      ?(z_off = 0.)
      ?(thickness = 3.5)
      ?(clearance = 2.)
      ?(n_steps = `PerZ 4.)
      ?(n_facets = 1)
      ?(north_lookup = fun i -> if i = 0 then Yes else No)
      ?(south_lookup = fun i -> if i = 0 then Yes else if i = 2 then Eye else No)
      ?(west = Yes)
      ?(east = No)
      ?(eyelet_config = Eyelet.m4_config)
      Plate.{ thumb = { config = { n_keys; _ }; keys; _ }; _ }
    =
    let siding =
      Wall.poly_siding ~d1 ~d2 ~z_off ~thickness ~clearance ~n_steps ~n_facets
    in
    let bez_wall = function
      | No  -> fun _ _ -> None
      | Yes -> fun side i -> Some (siding side i)
      | Eye -> fun side i -> Some (siding ~eyelet_config side i)
    in
    { keys =
        Map.mapi
          ~f:(fun ~key:i ~data ->
            { north = bez_wall (north_lookup i) `North data
            ; south = bez_wall (south_lookup i) `South data
            } )
          keys
    ; sides =
        { west = bez_wall west `West (Map.find_exn keys 0)
        ; east = bez_wall east `East (Map.find_exn keys (n_keys - 1))
        }
    }

  let to_scad { keys; sides = { west; east } } =
    let prepend wall = Util.prepend_opt_map ~f:(fun w -> Wall.to_scad w) wall in
    Scad.union
    @@ Map.fold
         ~init:(prepend west [] |> prepend east)
         ~f:(fun ~key:_ ~data:{ north; south } acc -> prepend north acc |> prepend south)
         keys

  let collect_screws ?(init = []) { keys; sides = { west; east } } =
    let prepend wall = Util.prepend_opt @@ Option.bind ~f:(fun w -> w.Wall.screw) wall in
    Map.fold
      ~init:(prepend west init |> prepend east)
      ~f:(fun ~key:_ ~data:{ north; south } acc -> prepend north acc |> prepend south)
      keys
end

type t =
  { body : Body.t
  ; thumb : Thumb.t
  }
[@@deriving scad]

let to_scad { body; thumb } = Scad.union [ Body.to_scad body; Thumb.to_scad thumb ]

let collect_screws { body; thumb } =
  Body.collect_screws ~init:(Thumb.collect_screws thumb) body
