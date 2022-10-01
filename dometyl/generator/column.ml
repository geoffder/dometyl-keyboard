open Scad_ml
open Syntax

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

type config =
  { key : Key.t
  ; n_keys : int
  ; curve : int -> Key.t -> Key.t
  }

type t =
  { config : config [@scad.ignore]
  ; scad : Scad.d3
  ; keys : Key.t IMap.t
  ; joins : Join.t IMap.t
  }
[@@deriving scad]

let make ?(join_ax = `NS) ~n_keys ~curve ~caps key =
  let place_key keys i =
    let cap =
      let p =
        V3.(
          (Key.normal key *$ (key.config.cap_height +. (key.config.thickness /. 2.)))
          +@ key.origin)
      in
      Option.some @@ Scad.translate p (caps i)
    in
    IMap.add i (curve i { key with cap }) keys
  in
  let get_start, get_dest =
    let get s key = Key.Faces.face key.Key.faces s
    and fs = key.Key.faces in
    match join_ax with
    | `NS ->
      (* fudge faces in for union *)
      let s = V3.(fs.north.normal *$ 0.01) in
      get `North >> Key.Face.translate (V3.neg s), get `South >> Key.Face.translate s
    | `EW ->
      let s = V3.(fs.east.normal *$ 0.01) in
      get `East >> Key.Face.translate (V3.neg s), get `West >> Key.Face.translate s
  in
  let join_keys (a : Key.t) (b : Key.t) =
    Mesh.to_scad @@ Mesh.hull (List.rev_append (get_start a).path (get_dest b).path)
  in
  let keys = List.fold_left place_key IMap.empty (List.init n_keys Fun.id) in
  let joins =
    let f key k1 m =
      match IMap.find_opt (key + 1) keys with
      | None -> m
      | Some k2 ->
        let open V3 in
        let face1 = get_start k1
        and face2 = get_dest k2 in
        let scad = join_keys k1 k2
        and dir1 = Key.Face.direction face1
        and dir2 = Key.Face.direction face2 in
        let west =
          Mesh.of_rows
            [ [ face1.points.top_left
              ; face1.points.bot_left
              ; face2.points.bot_right
              ; face2.points.top_right
              ]
            ; [ face1.points.top_left +@ (dir1 *$ -0.01)
              ; face1.points.bot_left +@ (dir1 *$ -0.01)
              ; face2.points.bot_right +@ (dir2 *$ 0.01)
              ; face2.points.top_right +@ (dir2 *$ 0.01)
              ]
            ]
          |> Mesh.to_scad
        and east =
          Mesh.of_rows
            [ [ face1.points.top_right
              ; face1.points.bot_right
              ; face2.points.bot_left
              ; face2.points.top_left
              ]
            ; [ face1.points.top_right +@ (dir1 *$ 0.01)
              ; face1.points.bot_right +@ (dir1 *$ 0.01)
              ; face2.points.bot_left +@ (dir2 *$ -0.01)
              ; face2.points.top_left +@ (dir2 *$ -0.01)
              ]
            ]
          |> Mesh.to_scad
        in
        IMap.add key Join.{ scad; faces = { west; east } } m
    in
    IMap.fold f keys IMap.empty
  in
  let scad =
    Scad.union
      (IMap.fold
         (fun _key data l -> data.Key.scad :: l)
         keys
         (IMap.fold (fun _key data acc -> data.Join.scad :: acc) joins []) )
  in
  { config = { key; n_keys; curve }; scad; keys; joins }

let to_scad t = t.scad
