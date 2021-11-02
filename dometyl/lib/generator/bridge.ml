open Base
open Scad_ml

type t = Scad.d3

let slide ?(d1 = 0.5) ?(d2 = 1.0) ~ortho scad =
  let a = Scad.translate (Vec3.map (( *. ) d1) ortho) scad
  and b = Scad.translate (Vec3.map (( *. ) d2) ortho) scad in
  Scad.hull [ scad; a ], Scad.hull [ a; b ]

(* NOTE: changed to in_d and out_d params (out from lower, and in to upper). Need to
   be configurable based on users clearance concerns. Need to explain well in the
   docs. The out needs d1 and d2 to allow shift outward before hulling up for key
   clearance. Since the in_d is inside the key, only one is relevant. If it is too
   large, it will interfere with needed switch clearance (like amoeba, hotswap). *)
let keys
    ?(start = `East)
    ?(dest = `West)
    ?(in_d = 0.25)
    ?out_d1
    ?out_d2
    (k1 : _ KeyHole.t)
    (k2 : _ KeyHole.t) =
  let aux (in_side, (inney : _ KeyHole.t)) (out_side, outey) =
    let out_a, out_b =
      slide
        ?d1:out_d1
        ?d2:out_d2
        ~ortho:(KeyHole.orthogonal outey out_side)
        (KeyHole.Faces.face outey.faces out_side).scad
    in
    let _, in_b =
      slide
        ~d1:0.
        ~d2:(Float.neg in_d)
        ~ortho:(KeyHole.orthogonal inney in_side)
        (KeyHole.Faces.face inney.faces in_side).scad
    in
    Scad.union [ Scad.hull [ in_b; out_b ]; out_a ]
  and face1 = KeyHole.Faces.face k1.faces start
  and face2 = KeyHole.Faces.face k2.faces dest in
  if Float.(Vec3.get_z face1.points.centre > Vec3.get_z face2.points.centre) then
    aux (start, k1) (dest, k2)
  else aux (dest, k2) (start, k1)

let cols ?(ax = `EW) ?(in_d = 0.25) ?out_d1 ?out_d2 ~columns a_i b_i =
  let a : _ Column.t = Map.find_exn columns a_i
  and b = Map.find_exn columns b_i
  and bookends keys join_idx = Map.find_exn keys join_idx, Map.find_exn keys (join_idx + 1)
  and start, dest =
    match ax with
    | `EW -> `East, `West
    | `NS -> `South, `North
  in
  let key_folder ~key:_ ~data hulls =
    match data with
    | `Both (a', b') -> keys ~start ~dest ~in_d ?out_d1 ?out_d2 a' b' :: hulls
    | _              -> hulls
  and join_folder ~key ~data hulls =
    let huller ~low_west (out_last, out_next, out_join) (in_last, in_next, in_join) =
      let mean_ortho side last next =
        Vec3.(normalize (KeyHole.orthogonal last side <+> KeyHole.orthogonal next side))
      and in_side, out_side = if low_west then dest, start else start, dest in
      let out_ortho = mean_ortho out_side out_last out_next
      and in_ortho = mean_ortho in_side in_last in_next in
      let out_a, out_b = slide ?d1:out_d1 ?d2:out_d2 ~ortho:out_ortho out_join
      and _, in_b = slide ~d1:0. ~d2:(Float.neg in_d) ~ortho:in_ortho in_join in
      Scad.union [ Scad.hull [ in_b; out_b ]; out_a ]
    in
    match data with
    | `Both ((w_join : Column.Join.t), (e_join : Column.Join.t)) ->
      let w_last, w_next = bookends a.keys key and e_last, e_next = bookends b.keys key in
      let w_set =
        ( w_last
        , w_next
        , match ax with
          | `EW -> w_join.faces.east
          | `NS -> w_join.faces.west )
      and e_set =
        ( e_last
        , e_next
        , match ax with
          | `EW -> e_join.faces.west
          | `NS -> e_join.faces.east )
      in
      let w_z =
        Vec3.(
          get_z
          @@ mean
               KeyHole.Faces.
                 [ (face w_last.faces start).points.centre
                 ; (face w_next.faces start).points.centre
                 ])
      and e_z =
        Vec3.(
          get_z
          @@ mean
               KeyHole.Faces.
                 [ (face e_last.faces dest).points.centre
                 ; (face e_next.faces dest).points.centre
                 ])
      in
      if Float.(w_z > e_z) then huller ~low_west:false e_set w_set :: hulls
      else huller ~low_west:true w_set e_set :: hulls
    | _ -> hulls
  in
  Scad.union
    (Map.fold2
       ~f:key_folder
       ~init:(Map.fold2 ~f:join_folder ~init:[] a.joins b.joins)
       a.keys
       b.keys )
