open Scad_ml
open Path3

let value_map_opt ~default f = function
  | Some a -> f a
  | None   -> default

let array_of_list_rev l =
  match l with
  | []       -> [||]
  | hd :: tl ->
    let len = 1 + List.length tl in
    let a = Array.make len hd
    and r = ref tl in
    (* Start at [len - 2] as [make] has placed [hd] at [t.(len - 1)]. *)
    for i = len - 2 downto 0 do
      match !r with
      | []       -> assert false
      | hd :: tl ->
        a.(i) <- hd;
        r := tl
    done;
    a

let to_transforms
    ?(initial = v3 0. 0. 1.)
    ?(euler = false)
    ?scale_ez
    ?twist_ez
    ?scale
    ?twist
    path
  =
  let p = Array.of_list path in
  let len = Array.length p
  and id _ = Affine3.id in
  let rel_pos =
    if Option.(is_some scale || is_some twist)
    then (
      let a = Array.of_list @@ cummulative_length path in
      for i = 0 to len - 1 do
        a.(i) <- a.(i) /. a.(len - 1)
      done;
      Array.get a )
    else Fun.const 0.
  in
  if len < 2 then invalid_arg "Invalid path (too few points).";
  let scaler = value_map_opt ~default:id (scaler ?ez:scale_ez) scale
  and twister = value_map_opt ~default:id (twister ?ez:twist_ez) twist
  and transformer =
    if euler
    then (
      let m = Quaternion.(to_affine @@ of_euler Float.(v3 (pi /. 2.) 0. (pi /. 2.))) in
      fun i ->
        let { x = dx; y = dy; z = dz } =
          if i = 0
          then V3.(p.(1) -@ p.(0))
          else if i = len - 1
          then V3.(p.(i) -@ p.(i - 1))
          else V3.(p.(i + 1) -@ p.(i - 1))
        in
        let ay = Float.atan2 dz (Float.sqrt ((dx *. dx) +. (dy *. dy)))
        and az = Float.atan2 dy dx in
        let q = Quaternion.of_euler (v3 0. (-.ay) az) in
        Affine3.(m %> Quaternion.(to_affine ~trans:p.(i) q)) )
    else (
      let accum_qs =
        let local i =
          let p1 = p.(i)
          and p2 = p.(i + 1)
          and p3 = p.(i + 2) in
          Quaternion.align V3.(normalize (p2 -@ p1)) V3.(normalize (p3 -@ p2))
        in
        match List.init (len - 2) local with
        | []       -> [| Quaternion.id |]
        | [ q ]    -> [| q; Quaternion.id |]
        | hd :: tl ->
          let f (acc, qs) m =
            let q = Quaternion.mul m acc in
            q, q :: qs
          in
          let _, qs = List.fold_left f (hd, [ hd; Quaternion.id ]) tl in
          array_of_list_rev qs
      in
      let init =
        let cardinal =
          (* Determine an appropriate axis to pre-align the 2d shape with
                 (from normal of {x = 0.; y = 0.; z = 1.}), BEFORE alignment
                 with the initial tangent of the path. Adjust for sign of major
                 axes to prevent inconsistent flipping. *)
          let similarity a b = V3.dot a b /. V3.(norm a *. norm b)
          and n = V3.(normalize (p.(1) -@ p.(0))) in
          let z = similarity n (v3 0. 0. 1.)
          and x = similarity n (v3 1. 0. 0.)
          and y = similarity n (v3 0. 1. 0.) in
          let abs_x = Float.abs x
          and abs_y = Float.abs y
          and abs_z = Float.abs z
          and sgn_x = Math.sign x
          and sgn_y = Math.sign y
          and sgn_z = Math.sign z in
          let comp a b =
            if Float.compare (Float.abs (a -. b)) 0.01 = 1 then Float.compare a b else 0
          in
          match comp abs_x abs_y, comp abs_x abs_z, comp abs_y abs_z with
          | 1, 1, _   -> v3 sgn_x 0. 0. (* x-axis *)
          | -1, _, 1  -> v3 0. sgn_y 0. (* y-axis *)
          | 0, -1, -1 -> v3 0. 0. sgn_z (* xy equal, but less than z *)
          | 0, _, _   -> v3 0. sgn_y 0. (* xy equal, roughly following plane *)
          | _         -> v3 0. 0. sgn_z
        in
        let d = V3.normalize V3.(p.(1) -@ p.(0)) in
        if V3.approx initial (v3 0. 0. 1.)
        then
          Quaternion.(to_affine @@ mul (align cardinal d) (align initial cardinal))
          (* else Affine3.id *)
          (* else Affine3.align (v3 0. 0. 1.) initial *)
          (* HACK: may want to consider adding the ability to supply an
                 orientation/alignment vector for non-euler. Would be a variant
                 instead of bool then e.g. [ `Euler | `Default | `Align of V3.t ] *)
        else Affine3.align initial (v3 0. 0. 1.)
      in
      fun i ->
        if i = 0
        then Affine3.(init %> translate p.(0))
        else Affine3.(init %> Quaternion.(to_affine ~trans:p.(i) accum_qs.(i - 1))) )
  in
  let f i = Affine3.(scaler (rel_pos i) %> twister (rel_pos i) %> transformer i) in
  List.init len f
