open! Base
open! Scad_ml

type presence =
  | No
  | Yes
  | Screw

module Body = struct
  module Cols = struct
    type col =
      { north : Wall.t option
      ; south : Wall.t option
      }

    type t = col Map.M(Int).t

    let make
        ?(d1 = 2.)
        ?(d2 = 5.)
        ?(z_off = 0.)
        ?(thickness = 3.5)
        ?(clearance = 2.5)
        ?(n_steps = `Flat 4)
        ?(north_lookup = fun i -> if i = 2 || i = 4 then Screw else Yes)
        ?(south_lookup =
          function
          | i when i = 3 -> Screw
          | i when i > 1 -> Yes
          | _ -> No)
        ?(screw_config = Screw.default_config)
        Plate.{ config = { spacing; _ }; columns; _ }
      =
      let drop =
        Wall.column_drop ~spacing ~columns ~z_off ~thickness ~clearance ~n_steps ~d1 ~d2
      in
      let bez_wall = function
        | No    -> fun _ _ -> None
        | Yes   -> fun side i -> Some (drop side i)
        | Screw -> fun side i -> Some (drop ~screw_config side i)
      in
      Map.mapi
        ~f:(fun ~key:i ~data:_ ->
          { north = bez_wall (north_lookup i) `North i
          ; south = bez_wall (south_lookup i) `South i
          } )
        columns

    let get t = function
      | `N -> t.north
      | `S -> t.south

    let col_to_scad col =
      Model.union
        (List.filter_map ~f:(Option.map ~f:Wall.to_scad) [ col.north; col.south ])

    let to_scad t =
      Model.union (Map.fold ~init:[] ~f:(fun ~key:_ ~data l -> col_to_scad data :: l) t)
  end

  module Sides = struct
    type t =
      { west : Wall.t Map.M(Int).t
      ; east : Wall.t Map.M(Int).t
      }

    let make
        ?(d1 = 2.)
        ?(d2 = 5.)
        ?(z_off = 0.)
        ?(thickness = 3.5)
        ?(clearance = 2.5)
        ?(n_steps = `Flat 4)
        ?(west_lookup = fun i -> if i = 0 then Screw else No)
        ?(east_lookup = fun _ -> No)
        ?(screw_config = Screw.default_config)
        Plate.{ columns; _ }
      =
      let west_col = Map.find_exn columns 0
      and _, east_col = Map.max_elt_exn columns
      and siding = Wall.poly_siding ~d1 ~d2 ~z_off ~thickness ~clearance ~n_steps in
      let sider side ~key ~data m =
        let lookup =
          match side with
          | `West -> west_lookup
          | `East -> east_lookup
        in
        match lookup key with
        | Yes   -> Map.add_exn ~key ~data:(siding side data) m
        | Screw -> Map.add_exn ~key ~data:(siding ~screw_config side data) m
        | No    -> m
      in
      { west = Map.fold ~init:(Map.empty (module Int)) ~f:(sider `West) west_col.keys
      ; east = Map.fold ~init:(Map.empty (module Int)) ~f:(sider `East) east_col.keys
      }

    let to_scad t =
      let f ~key:_ ~data l = Wall.to_scad data :: l in
      Model.union (Map.fold ~init:(Map.fold ~init:[] ~f t.west) ~f t.east)
  end

  type t =
    { cols : Cols.t
    ; sides : Sides.t
    }

  (* TODO: rough draft. This impl does not allow for different settings between cols
   * and siding. Is that fine? Or should I add more params here, or just make separately? *)
  let make
      ?d1
      ?d2
      ?z_off
      ?thickness
      ?n_steps
      ?north_lookup
      ?south_lookup
      ?west_lookup
      ?east_lookup
      plate
    =
    { cols =
        Cols.make ?d1 ?d2 ?z_off ?thickness ?n_steps ?north_lookup ?south_lookup plate
    ; sides =
        Sides.make ?d1 ?d2 ?z_off ?thickness ?n_steps ?west_lookup ?east_lookup plate
    }

  let to_scad t = Model.union [ Cols.to_scad t.cols; Sides.to_scad t.sides ]
end

module Thumb = struct
  type key =
    { north : Wall.t option
    ; south : Wall.t option
    }

  type sides =
    { west : Wall.t option
    ; east : Wall.t option
    }

  type t =
    { keys : key Map.M(Int).t
    ; sides : sides
    }

  let make
      ?(d1 = 1.)
      ?(d2 = 3.)
      ?(z_off = 0.)
      ?(thickness = 3.5)
      ?(clearance = 4.)
      ?(n_steps = `PerZ 4.)
      ?(north_lookup = fun i -> if i = 0 then Yes else No)
      ?(south_lookup = fun i -> if i = 0 then Yes else if i = 2 then Screw else No)
      ?(west = Yes)
      ?(east = No)
      ?(screw_config = Screw.default_config)
      Plate.{ thumb = { config = { n_keys; _ }; keys; _ }; _ }
    =
    let siding = Wall.poly_siding ~d1 ~d2 ~z_off ~thickness ~clearance ~n_steps in
    let bez_wall = function
      | No    -> fun _ _ -> None
      | Yes   -> fun side i -> Some (siding side i)
      | Screw -> fun side i -> Some (siding ~screw_config side i)
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
    let prepend wall l =
      Option.value_map ~default:l ~f:(fun w -> Wall.to_scad w :: l) wall
    in
    Model.union
    @@ Map.fold
         ~init:(prepend west [] |> prepend east)
         ~f:(fun ~key:_ ~data:{ north; south } acc -> prepend north acc |> prepend south)
         keys
end

type t =
  { body : Body.t
  ; thumb : Thumb.t
  }

let to_scad { body; thumb } = Model.union [ Body.to_scad body; Thumb.to_scad thumb ]
