open Base
open Scad_ml

type 'k config =
  { key : 'k KeyHole.t
  ; n_keys : int
  ; curve : int -> 'k KeyHole.t -> 'k KeyHole.t
  }

type 'k t =
  { config : 'k config
  ; scad : Model.t
  ; keys : 'k KeyHole.t Map.M(Int).t
  ; joins : Model.t Map.M(Int).t
  }

let translate p t =
  { t with
    scad = Model.translate p t.scad
  ; keys = Map.map ~f:(KeyHole.translate p) t.keys
  ; joins = Map.map ~f:(Model.translate p) t.joins
  }

let rotate r t =
  { t with
    scad = Model.rotate r t.scad
  ; keys = Map.map ~f:(KeyHole.rotate r) t.keys
  ; joins = Map.map ~f:(Model.rotate r) t.joins
  }

let rotate_about_pt r p t =
  { t with
    scad = Model.rotate_about_pt r p t.scad
  ; keys = Map.map ~f:(KeyHole.rotate_about_pt r p) t.keys
  ; joins = Map.map ~f:(Model.rotate_about_pt r p) t.joins
  }

let make ~key ~n_keys ~curve =
  let place_key keys i = Map.add_exn ~key:i ~data:(curve i key) keys in
  let join_keys (a : 'k KeyHole.t) (b : 'k KeyHole.t) =
    Model.hull [ a.faces.north.scad; b.faces.south.scad ]
  in
  let keys =
    List.fold (List.range 0 n_keys) ~init:(Map.empty (module Int)) ~f:place_key
  in
  let joins =
    Map.fold
      ~f:(fun ~key ~data:k1 m ->
        match Map.find keys (key + 1) with
        | None    -> m
        | Some k2 -> Map.add_exn m ~key ~data:(join_keys k1 k2) )
      ~init:(Map.empty (module Int))
      keys
  in
  let scad =
    Model.union
      (Map.fold ~f:(fun ~key:_ ~data l -> data.scad :: l) ~init:(Map.data joins) keys)
  in
  { config = { key; n_keys; curve }; scad; keys; joins }
