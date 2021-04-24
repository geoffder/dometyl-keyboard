open Base
open Scad_ml

module KeyHole = struct
  type t =
    { scad : Model.t
    ; north_face : Model.t
    ; south_face : Model.t
    ; east_face : Model.t
    ; west_face : Model.t
    }

  let map ~f t =
    { scad = f t.scad
    ; north_face = f t.north_face
    ; south_face = f t.south_face
    ; east_face = f t.east_face
    ; west_face = f t.west_face
    }

  let outer_w = 19.
  let inner_w = 14.
  let thickness = 4.

  let side_face =
    Model.rotate
      (Math.pi /. 2., 0., 0.)
      (Model.cube ~center:true (outer_w, thickness, 0.1))

  let north_face = Model.translate (0., outer_w /. 2., 0.) side_face
  let south_face = Model.translate (0., outer_w /. -2., 0.) side_face

  let west_face =
    side_face
    |> Model.rotate (0., 0., Math.pi /. 2.)
    |> Model.translate (outer_w /. -2., 0., 0.)

  let east_face =
    side_face
    |> Model.rotate (0., 0., Math.pi /. 2.)
    |> Model.translate (outer_w /. 2., 0., 0.)

  let scad =
    let outer = Model.cube ~center:true (outer_w, outer_w, thickness) in
    let inner = Model.cube ~center:true (inner_w, inner_w, thickness +. 0.1) in
    Model.difference outer [ inner ]

  let t = { scad; north_face; south_face; east_face; west_face }
end

module Column = struct
  type t =
    { scad : Model.t
    ; keys : KeyHole.t Map.M(Int).t
    ; joins : Model.t Map.M(Int).t
    }

  let map ~f t =
    { scad = f t.scad
    ; keys = Map.map ~f:(KeyHole.map ~f) t.keys
    ; joins = Map.map ~f t.joins
    }

  let n_keys = 3
  let angle = Math.pi /. 12.
  let radius = 85.

  let place_keys keys i =
    let next =
      KeyHole.map
        ~f:(Util.rotate_about_pt (angle *. Int.to_float i, 0., 0.) (0., 0., -.radius))
        KeyHole.t
    in
    Map.add_exn ~key:i ~data:next keys

  let join_keys (a : KeyHole.t) (b : KeyHole.t) =
    Model.hull [ a.north_face; b.south_face ]

  let keys = List.fold (List.range 0 n_keys) ~init:(Map.empty (module Int)) ~f:place_keys

  let joins =
    Map.fold
      ~f:(fun ~key ~data:k1 m ->
        match Map.find keys (key + 1) with
        | None    -> m
        | Some k2 -> Map.add_exn m ~key ~data:(join_keys k1 k2) )
      ~init:(Map.empty (module Int))
      keys

  let scad =
    Model.union
      (Map.fold ~f:(fun ~key:_ ~data l -> data.scad :: l) ~init:(Map.data joins) keys)

  let t = { scad; keys; joins }
end
