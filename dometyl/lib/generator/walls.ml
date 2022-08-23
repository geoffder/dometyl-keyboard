open! Base
open! Scad_ml

type presence =
  | No
  | Yes
  | Eye

module Side = struct
  type t = (Wall.t Map.M(Int).t[@scad.d3]) [@@deriving scad_jane]
  type config = int -> Wall.config option
end

module Sides = struct
  type t =
    { west : Side.t [@scad.d3]
    ; north : Side.t
    ; east : Side.t
    ; south : Side.t
    }
  [@@deriving scad]

  let manual_body ?(spacing = 1.) ~west ~north ~east ~south (columns : Columns.t) =
    { west =
        Map.filter_mapi
          ~f:(fun ~key ~data ->
            Option.map ~f:(fun c -> Wall.poly_of_config c `West data) (west key) )
          (Map.find_exn columns 0).keys
    ; north =
        Map.filter_mapi
          ~f:(fun ~key ~data:_ ->
            Option.map
              ~f:(fun c -> Wall.drop_of_config ~spacing ~columns c `North key)
              (north key) )
          columns
    ; east =
        Map.filter_mapi
          ~f:(fun ~key ~data ->
            Option.map ~f:(fun c -> Wall.poly_of_config c `East data) (east key) )
          (snd @@ Map.max_elt_exn columns).keys
    ; south =
        Map.filter_mapi
          ~f:(fun ~key ~data:_ ->
            Option.map
              ~f:(fun c -> Wall.drop_of_config ~spacing ~columns c `South key)
              (south key) )
          columns
    }

  let manual_thumb ~west ~north ~east ~south (columns : Columns.t) =
    { west =
        Map.filter_mapi
          ~f:(fun ~key ~data ->
            let%bind.Option _, k = Map.min_elt data.keys in
            Option.map ~f:(fun c -> Wall.poly_of_config c `West k) (west key) )
          columns
    ; north =
        Map.filter_mapi
          ~f:(fun ~key ~data ->
            Option.map ~f:(fun c -> Wall.poly_of_config c `North data) (north key) )
          (snd @@ Map.min_elt_exn columns).keys
    ; east =
        Map.filter_mapi
          ~f:(fun ~key ~data ->
            let%bind.Option k = Map.find data.keys 0 in
            Option.map ~f:(fun c -> Wall.poly_of_config c `East k) (east key) )
          columns
    ; south =
        Map.filter_mapi
          ~f:(fun ~key ~data ->
            Option.map ~f:(fun c -> Wall.poly_of_config c `South data) (south key) )
          (snd @@ Map.max_elt_exn columns).keys
    }

  let auto
      ?(d1 = 2.)
      ?(d2 = 5.)
      ?(north_clearance = 2.5)
      ?(south_clearance = 2.5)
      ?(side_clearance = 3.0)
      ?(n_steps = `Flat 4)
      ?scale:s
      ?scale_ez
      ?index_scale
      ?(north_lookup = fun i -> if i = 2 || i = 4 then Eye else Yes)
      ?(south_lookup =
        function
        | i when i = 3 -> Eye
        | i when i > 1 -> Yes
        | _ -> No)
      ?(west_lookup = fun i -> if i = 0 then Eye else No)
      ?(east_lookup = fun _ -> No)
      ?(eyelet_config = Eyelet.m4_config)
      ?(spacing = 1.)
      ?(thumb = false)
      (columns : Columns.t)
    =
    let present m i conf = function
      | Yes -> Map.add_exn m ~key:i ~data:conf
      | Eye ->
        Map.add_exn m ~key:i ~data:Wall.{ conf with eyelet_config = Some eyelet_config }
      | No -> m
    and conf =
      Wall.
        { d1
        ; d2
        ; clearance = side_clearance
        ; n_steps
        ; scale = s
        ; scale_ez
        ; eyelet_config = None
        }
    and init = Map.empty (module Int) in
    let west =
      Map.find
      @@ List.fold
           ~init
           ~f:(fun m i -> present m i conf (west_lookup i))
           (if thumb then Map.keys columns else Map.keys (Map.find_exn columns 0).keys)
    and east =
      Map.find
      @@ List.fold
           ~init
           ~f:(fun m i -> present m i conf (east_lookup i))
           ( if thumb
           then Map.keys columns
           else Map.keys (snd @@ Map.min_elt_exn columns).keys )
    and north =
      let index = Option.first_some index_scale s in
      let f m i =
        present
          m
          i
          { conf with clearance = north_clearance; scale = (if i < 2 then index else s) }
          (north_lookup i)
      in
      Map.find
      @@ List.fold
           ~init
           ~f
           ( if thumb
           then Map.keys (snd @@ Map.min_elt_exn columns).keys
           else Map.keys columns )
    and south =
      Map.find
      @@ List.fold
           ~init
           ~f:(fun m i ->
             present m i { conf with clearance = south_clearance } (south_lookup i) )
           ( if thumb
           then Map.keys (snd @@ Map.max_elt_exn columns).keys
           else Map.keys columns )
    in
    if thumb
    then manual_thumb ~west ~north ~east ~south columns
    else manual_body ~west ~north ~east ~south ~spacing columns

  let fold ~init ~f (t : t) =
    let init = Map.fold ~init ~f t.west in
    let init = Map.fold ~init ~f t.north in
    let init = Map.fold ~init ~f t.east in
    Map.fold ~init ~f t.south

  let get t = function
    | `N -> t.north
    | `E -> t.east
    | `S -> t.south
    | `W -> t.west

  let to_scad t =
    let f ~key:_ ~data l = Wall.to_scad data :: l in
    Scad.union_3d (fold ~init:[] ~f t)

  let collect_screws ?(init = []) (t : t) =
    let f ~key:_ ~data l =
      Option.value_map ~default:l ~f:(fun s -> s :: l) data.Wall.screw
    in
    fold ~init ~f t
end

let auto_body
    ?d1
    ?d2
    ?north_clearance
    ?south_clearance
    ?side_clearance
    ?n_steps
    ?scale
    ?scale_ez
    ?index_scale
    ?north_lookup
    ?south_lookup
    ?west_lookup
    ?east_lookup
    ?eyelet_config
    Plate.{ config = { spacing; _ }; body; _ }
  =
  Sides.auto
    ?d1
    ?d2
    ?north_clearance
    ?south_clearance
    ?side_clearance
    ?n_steps
    ?scale
    ?scale_ez
    ?index_scale
    ?north_lookup
    ?south_lookup
    ?west_lookup
    ?east_lookup
    ?eyelet_config
    ~spacing
    body

let auto_thumb
    ?(d1 = 1.)
    ?(d2 = 3.)
    ?(north_clearance = 2.5)
    ?(south_clearance = 2.5)
    ?(side_clearance = 3.0)
    ?(n_steps = `PerZ 4.)
    ?scale
    ?scale_ez
    ?(north_lookup = fun i -> if i = 0 then Yes else No)
    ?(south_lookup = fun i -> if i = 0 then Yes else if i = 2 then Eye else No)
    ?(west_lookup = fun i -> if i = 0 then Yes else No)
    ?(east_lookup = fun _ -> No)
    ?eyelet_config
    Plate.{ thumb; _ }
  =
  Sides.auto
    ~d1
    ~d2
    ~north_clearance
    ~south_clearance
    ~side_clearance
    ~n_steps
    ?scale
    ?scale_ez
    ~north_lookup
    ~south_lookup
    ~west_lookup
    ~east_lookup
    ?eyelet_config
    ~thumb:true
    thumb

type t =
  { body : Sides.t [@scad.d3]
  ; thumb : Sides.t
  }
[@@deriving scad]

let make ~body ~thumb = { body; thumb }

let manual
    ~body_west
    ~body_north
    ~body_east
    ~body_south
    ~thumb_south
    ~thumb_north
    ~thumb_east
    ~thumb_west
    Plate.{ config = { spacing; _ }; body; thumb; _ }
  =
  { body =
      Sides.manual_body
        ~west:body_west
        ~north:body_north
        ~east:body_east
        ~south:body_south
        ~spacing
        body
  ; thumb =
      Sides.manual_thumb
        ~west:thumb_west
        ~north:thumb_north
        ~east:thumb_east
        ~south:thumb_south
        thumb
  }

let to_scad { body; thumb } = Scad.union [ Sides.to_scad body; Sides.to_scad thumb ]

let collect_screws { body; thumb } =
  Sides.collect_screws ~init:(Sides.collect_screws thumb) body
