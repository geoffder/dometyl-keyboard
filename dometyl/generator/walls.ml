open! Scad_ml
open! Syntax

module Side = struct
  type t = (Wall.t IMap.t[@scad.d3]) [@@deriving scad]
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

  let manual_body ~west ~north ~east ~south (columns : Columns.t) =
    { west =
        IMap.filter_mapi
          (fun key data -> Option.map (fun c -> Wall.of_config c `West data) (west key))
          (IMap.find 0 columns).keys
    ; north =
        IMap.filter_mapi
          (fun key data ->
            let* _, k = IMap.max_binding_opt data.Column.keys in
            Option.map (fun c -> Wall.of_config c `North k) (north key) )
          columns
    ; east =
        IMap.filter_mapi
          (fun key data -> Option.map (fun c -> Wall.of_config c `East data) (east key))
          (snd @@ IMap.max_binding columns).keys
    ; south =
        IMap.filter_mapi
          (fun key data ->
            let* _, k = IMap.min_binding_opt data.Column.keys in
            Option.map (fun c -> Wall.of_config c `South k) (south key) )
          columns
    }

  let manual_thumb ~west ~north ~east ~south (columns : Columns.t) =
    { west =
        IMap.filter_mapi
          (fun key data ->
            let* _, k = IMap.min_binding_opt data.Column.keys in
            Option.map (fun c -> Wall.of_config c `West k) (west key) )
          columns
    ; north =
        IMap.filter_mapi
          (fun key data -> Option.map (fun c -> Wall.of_config c `North data) (north key))
          (snd @@ IMap.min_binding columns).keys
    ; east =
        IMap.filter_mapi
          (fun key data ->
            let* _, k = IMap.max_binding_opt data.Column.keys in
            Option.map (fun c -> Wall.of_config c `East k) (east key) )
          columns
    ; south =
        IMap.filter_mapi
          (fun key data -> Option.map (fun c -> Wall.of_config c `South data) (south key))
          (snd @@ IMap.max_binding columns).keys
    }

  let auto
      ?d1
      ?d2
      ?north_clearance
      ?south_clearance
      ?side_clearance
      ?n_steps
      ?min_step_dist
      ?scale:s
      ?scale_ez
      ?end_z
      ?index_scale
      ?(north_lookup = Fun.const true)
      ?(south_lookup = fun i -> i > 1)
      ?(west_lookup = fun i -> i = 0)
      ?(east_lookup = Fun.const false)
      ?(thumb = false)
      (columns : Columns.t)
    =
    let present m i conf = function
      | true -> IMap.add i conf m
      | false -> m
    and conf =
      Wall.
        { d1
        ; d2
        ; clearance = side_clearance
        ; n_steps
        ; min_step_dist
        ; scale = s
        ; scale_ez
        ; end_z
        }
    and init = IMap.empty in
    let west =
      Fun.flip IMap.find_opt
      @@ Seq.fold_left
           (fun m i -> present m i conf (west_lookup i))
           init
           (if thumb then IMap.keys columns else IMap.keys (IMap.find 0 columns).keys)
    and east =
      Fun.flip IMap.find_opt
      @@ Seq.fold_left
           (fun m i -> present m i conf (east_lookup i))
           init
           ( if thumb
           then IMap.keys columns
           else IMap.keys (snd @@ IMap.min_binding columns).keys )
    and north =
      let index = Util.first_some index_scale s in
      let f m i =
        let conf =
          { conf with clearance = north_clearance; scale = (if i < 2 then index else s) }
        in
        present m i conf (north_lookup i)
      in
      Fun.flip IMap.find_opt
      @@ Seq.fold_left
           f
           init
           ( if thumb
           then IMap.keys (snd @@ IMap.min_binding columns).keys
           else IMap.keys columns )
    and south =
      Fun.flip IMap.find_opt
      @@ Seq.fold_left
           (fun m i ->
             present m i { conf with clearance = south_clearance } (south_lookup i) )
           init
           ( if thumb
           then IMap.keys (snd @@ IMap.max_binding columns).keys
           else IMap.keys columns )
    in
    if thumb
    then manual_thumb ~west ~north ~east ~south columns
    else manual_body ~west ~north ~east ~south columns

  let fold f init (t : t) =
    let init = IMap.fold f t.west init in
    let init = IMap.fold f t.north init in
    let init = IMap.fold f t.east init in
    IMap.fold f t.south init

  let get t = function
    | `N -> t.north
    | `E -> t.east
    | `S -> t.south
    | `W -> t.west

  let to_scad t =
    let f _key data l = Wall.to_scad data :: l in
    Scad.union3 (fold f [] t)
end

let auto_body
    ?d1
    ?d2
    ?north_clearance
    ?south_clearance
    ?side_clearance
    ?n_steps
    ?min_step_dist
    ?scale
    ?scale_ez
    ?end_z
    ?index_scale
    ?north_lookup
    ?south_lookup
    ?west_lookup
    ?east_lookup
    Plate.{ body; _ }
  =
  Sides.auto
    ?d1
    ?d2
    ?north_clearance
    ?south_clearance
    ?side_clearance
    ?n_steps
    ?min_step_dist
    ?scale
    ?scale_ez
    ?end_z
    ?index_scale
    ?north_lookup
    ?south_lookup
    ?west_lookup
    ?east_lookup
    body

let auto_thumb
    ?d1
    ?d2
    ?north_clearance
    ?south_clearance
    ?side_clearance
    ?n_steps
    ?min_step_dist
    ?scale
    ?scale_ez
    ?end_z
    ?(north_lookup = fun i -> i = 0)
    ?(south_lookup = fun i -> i = 0 || i = 2)
    ?(west_lookup = fun i -> i = 0)
    ?(east_lookup = fun _ -> false)
    Plate.{ thumb; _ }
  =
  Sides.auto
    ?d1
    ?d2
    ?north_clearance
    ?south_clearance
    ?side_clearance
    ?n_steps
    ?min_step_dist
    ?scale
    ?scale_ez
    ?end_z
    ~north_lookup
    ~south_lookup
    ~west_lookup
    ~east_lookup
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
    Plate.{ body; thumb; _ }
  =
  { body =
      Sides.manual_body
        ~west:body_west
        ~north:body_north
        ~east:body_east
        ~south:body_south
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
