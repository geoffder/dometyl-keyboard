open Base
open Scad_ml

module Join = struct
  module Faces = struct
    type t =
      { west : Scad.d3
      ; east : Scad.d3
      }
    [@@deriving scad]

    let map ~f t = { west = f t.west; east = f t.east }

    let face t = function
      | `West -> t.west
      | `East -> t.east
  end

  type t =
    { scad : Scad.d3
    ; faces : Faces.t
    }
  [@@deriving scad]
end

type 'k config =
  { key : 'k KeyHole.t
  ; n_keys : int
  ; curve : int -> 'k KeyHole.t -> 'k KeyHole.t
  }

type 'k t =
  { config : 'k config [@scad.ignore]
  ; scad : Scad.d3
  ; keys : 'k KeyHole.t Map.M(Int).t
  ; joins : Join.t Map.M(Int).t
  }
[@@deriving scad_jane]

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
      Option.some @@ Scad.translate p (caps i)
    in
    Map.add_exn ~key:i ~data:(curve i { key with cap }) keys
  in
  let get_start, get_dest =
    let get s key = KeyHole.Faces.face key.KeyHole.faces s in
    match join_ax with
    | `NS -> get `North, get `South
    | `EW -> get `East, get `West
  in
  let join_keys (a : 'k KeyHole.t) (b : 'k KeyHole.t) =
    Scad.hull [ (get_start a).scad; (get_dest b).scad ]
  in
  let keys =
    List.fold (List.range 0 n_keys) ~init:(Map.empty (module Int)) ~f:place_key
  in
  let joins =
    let f ~key ~data:k1 m =
      match Map.find keys (key + 1) with
      | None    -> m
      | Some k2 ->
        let open Vec3 in
        let face1 = get_start k1 and face2 = get_dest k2 in
        let scad = join_keys k1 k2
        and dir1 = KeyHole.Face.direction face1
        and dir2 = KeyHole.Face.direction face2 in
        let west =
          Util.prism_exn
            [ face1.points.top_left
            ; face1.points.bot_left
            ; face2.points.bot_right
            ; face2.points.top_right
            ]
            [ face1.points.top_left <+> mul_scalar dir1 (-0.01)
            ; face1.points.bot_left <+> mul_scalar dir1 (-0.01)
            ; face2.points.bot_right <+> mul_scalar dir2 0.01
            ; face2.points.top_right <+> mul_scalar dir2 0.01
            ]
        and east =
          Util.prism_exn
            [ face1.points.top_right
            ; face1.points.bot_right
            ; face2.points.bot_left
            ; face2.points.top_left
            ]
            [ face1.points.top_right <+> mul_scalar dir1 0.01
            ; face1.points.bot_right <+> mul_scalar dir1 0.01
            ; face2.points.bot_left <+> mul_scalar dir2 (-0.01)
            ; face2.points.top_left <+> mul_scalar dir2 (-0.01)
            ]
        in
        Map.add_exn m ~key ~data:Join.{ scad; faces = { west; east } }
    in
    Map.fold ~f ~init:(Map.empty (module Int)) keys
  in
  let scad =
    Scad.union
      (Map.fold
         ~f:(fun ~key:_ ~data l -> data.scad :: l)
         ~init:(Map.fold ~f:(fun ~key:_ ~data acc -> data.scad :: acc) ~init:[] joins)
         keys )
  in
  { config = { key; n_keys; curve }; scad; keys; joins }

let to_scad t = t.scad
