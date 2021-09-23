open Base
open Scad_ml

module Join = struct
  module Faces = struct
    type t =
      { west : Model.t
      ; east : Model.t
      }

    let map ~f t = { west = f t.west; east = f t.east }
    let translate p = map ~f:(Model.translate p)
    let mirror ax = map ~f:(Model.mirror ax)
    let rotate r = map ~f:(Model.rotate r)
    let rotate_about_pt r p = map ~f:(Model.rotate_about_pt r p)

    let face t = function
      | `West -> t.west
      | `East -> t.east
  end

  type t =
    { scad : Model.t
    ; faces : Faces.t
    }

  let translate p t =
    { scad = Model.translate p t.scad; faces = Faces.translate p t.faces }

  let mirror ax t = { scad = Model.mirror ax t.scad; faces = Faces.mirror ax t.faces }
  let rotate r t = { scad = Model.rotate r t.scad; faces = Faces.rotate r t.faces }

  let rotate_about_pt r p t =
    { scad = Model.rotate_about_pt r p t.scad; faces = Faces.rotate_about_pt r p t.faces }
end

type 'k config =
  { key : 'k KeyHole.t
  ; n_keys : int
  ; curve : int -> 'k KeyHole.t -> 'k KeyHole.t
  }

type 'k t =
  { config : 'k config
  ; scad : Model.t
  ; keys : 'k KeyHole.t Map.M(Int).t
  ; joins : Join.t Map.M(Int).t
  }

let translate p t =
  { t with
    scad = Model.translate p t.scad
  ; keys = Map.map ~f:(KeyHole.translate p) t.keys
  ; joins = Map.map ~f:(Join.translate p) t.joins
  }

let mirror ax t =
  { t with
    scad = Model.mirror ax t.scad
  ; keys = Map.map ~f:(KeyHole.mirror ax) t.keys
  ; joins = Map.map ~f:(Join.mirror ax) t.joins
  }

let rotate r t =
  { t with
    scad = Model.rotate r t.scad
  ; keys = Map.map ~f:(KeyHole.rotate r) t.keys
  ; joins = Map.map ~f:(Join.rotate r) t.joins
  }

let rotate_about_pt r p t =
  { t with
    scad = Model.rotate_about_pt r p t.scad
  ; keys = Map.map ~f:(KeyHole.rotate_about_pt r p) t.keys
  ; joins = Map.map ~f:(Join.rotate_about_pt r p) t.joins
  }

let make ?(join_ax = `NS) ~n_keys ~curve ~caps key =
  let place_key keys i =
    let cap =
      let p =
        Vec3.(
          add
            (mul_scalar
               (KeyHole.normal key)
               (key.config.cap_height +. (key.config.thickness /. 2.)) )
            key.origin)
      in
      Option.some @@ Model.translate p (caps i)
    in
    Map.add_exn ~key:i ~data:(curve i { key with cap }) keys
  in
  let join_keys (a : 'k KeyHole.t) (b : 'k KeyHole.t) =
    match join_ax with
    | `NS -> Model.hull [ a.faces.north.scad; b.faces.south.scad ]
    | `EW -> Model.hull [ a.faces.east.scad; b.faces.west.scad ]
  in
  let keys =
    List.fold (List.range 0 n_keys) ~init:(Map.empty (module Int)) ~f:place_key
  in
  let joins =
    let f ~key ~data:k1 m =
      match Map.find keys (key + 1) with
      | None    -> m
      | Some k2 ->
        let scad = join_keys k1 k2 in
        let west = Model.difference scad [ Model.translate (0.01, 0., 0.) scad ]
        and east = Model.difference scad [ Model.translate (-0.01, 0., 0.) scad ] in
        Map.add_exn m ~key ~data:Join.{ scad; faces = { west; east } }
    in
    Map.fold ~f ~init:(Map.empty (module Int)) keys
  in
  let scad =
    Model.union
      (Map.fold
         ~f:(fun ~key:_ ~data l -> data.scad :: l)
         ~init:(Map.fold ~f:(fun ~key:_ ~data acc -> data.scad :: acc) ~init:[] joins)
         keys )
  in
  { config = { key; n_keys; curve }; scad; keys; joins }
