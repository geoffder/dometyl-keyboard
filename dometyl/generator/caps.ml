open! Scad_ml

(* TODO: This is an awful hack, also it lacks windows compatibility. Once it is
    possible to install directories of files
    (soon: https://github.com/ocaml/dune/pull/6139), should install the stl
    assets used by the generator, and have the models rule depend on @install *)
let path n =
  let cwd = Sys.getcwd () in
  let parts = String.split_on_char (String.get Filename.dir_sep 0) cwd in
  let rec loop acc = function
    | hd :: tl ->
      let acc = Printf.sprintf "%s%s%s" acc hd Filename.dir_sep in
      if String.equal hd "dometyl" then acc else loop acc tl
    | _ -> failwith "dometyl library expected in path"
  in
  let path = loop "/" parts in
  Printf.sprintf "%s/assets/caps/%s" path n

let color = Scad.color Color.DarkSlateBlue

module SA = struct
  (* Generated with https://github.com/rsheldiii/KeyV2 *)
  let path n = path (Printf.sprintf "KeyV2_SA/SA-%s.stl" n)
  let r0 = Scad.import3 (path "R0") |> color
  let r2 = Scad.import3 (path "R2") |> color
  let r3 = Scad.import3 (path "R3") |> color
  let r4 = Scad.import3 (path "R4") |> color
  let r5 = Scad.import3 (path "R5") |> color

  let row = function
    | 0 -> r4
    | 1 -> r3
    | 2 -> r2
    | _ -> r0

  let uniform _ = r3
end

module MT3 = struct
  (* STLs released by the designer matt3o (https://matt3o.com/) *)
  let path n = path (Printf.sprintf "MT3/MT3-%s.stl" n)
  let space_1u = Scad.import3 (path "1u-space") |> color
  let space_1_25u = Scad.import3 (path "1.25u-space") |> color
  let space_1_5u = Scad.import3 (path "1.5u-space") |> color
  let thumb_1u _ = space_1u
  let thumb_1_25u _ = space_1_25u
  let thumb_1_5u _ = space_1_5u
end

module Matty3 = struct
  (* Generated with https://github.com/mrebersv/KeyV2
     NOTE: Be mindful of clearance between the keys. The shape of real MT3 caps
     seem to have a bit more of a bulge. These can be close to touching (but not
     quite) and the caps will rub irl. *)
  let path n = path (Printf.sprintf "KeyV2_matty3/matty3-deep-%s.stl" n)
  let r0 = Scad.import3 (path "R0") |> color
  let r1 = Scad.import3 (path "R1") |> color
  let r2 = Scad.import3 (path "R2") |> color
  let r3 = Scad.import3 (path "R3") |> color
  let r4 = Scad.import3 (path "R4") |> color
  let r5 = Scad.import3 (path "R5") |> color

  let row = function
    | 0 -> r4
    | 1 -> r3
    | 2 -> r2
    | _ -> r1
end

module MBK = struct
  (* Reproduction by darryldh, found at https://www.thingiverse.com/thing:4564253 *)
  let mbk =
    Scad.import3 (path "darryldh_MBK/MBK_1u.stl") |> Scad.color Color.DarkSlateBlue

  let uniform _ = mbk
end

module Cherry = struct
  (* Generated with https://github.com/rsheldiii/KeyV2 *)
  let path n = path (Printf.sprintf "KeyV2_cherry/cherry-%s.stl" n)
  let r0 = Scad.import3 (path "R0") |> color
  let r2 = Scad.import3 (path "R2") |> color
  let r3 = Scad.import3 (path "R3") |> color
  let r4 = Scad.import3 (path "R4") |> color

  let row = function
    | 0 -> r4
    | 1 -> r3
    | 2 -> r2
    | _ -> r0
end

module OEM = struct
  (* Generated with https://github.com/rsheldiii/KeyV2 *)
  let path n = path (Printf.sprintf "KeyV2_OEM/OEM-%s.stl" n)
  let r1 = Scad.import3 (path "R1") |> color
  let r2 = Scad.import3 (path "R2") |> color
  let r3 = Scad.import3 (path "R3") |> color
  let r4 = Scad.import3 (path "R4") |> color
  let r5 = Scad.import3 (path "R5") |> color

  let row = function
    | 0 -> r4
    | 1 -> r3
    | 2 -> r2
    | 3 -> r1
    | _ -> r5
end

module DSA = struct
  (* Generated with https://github.com/rsheldiii/KeyV2 *)
  let r3 = Scad.import3 (path "KeyV2_DSA/DSA-R3.stl") |> color
  let uniform _ = r3
end

module DSS = struct
  (* Generated with https://github.com/rsheldiii/KeyV2 *)
  let path n = path (Printf.sprintf "KeyV2_DSS/DSS-%s.stl" n)
  let r1 = Scad.import3 (path "R1") |> color
  let r2 = Scad.import3 (path "R2") |> color
  let r3 = Scad.import3 (path "R3") |> color
  let r4 = Scad.import3 (path "R4") |> color
  let r5 = Scad.import3 (path "R5") |> color

  let row = function
    | 0 -> r4
    | 1 -> r3
    | 2 -> r2
    | 3 -> r1
    | _ -> r5
end

module DES = struct
  (* Generated with https://github.com/pseudoku/PseudoMakeMeKeyCapProfiles *)
  let path n = path (Printf.sprintf "Pseudoku_DES/%s.stl" n)
  let r1 = Scad.import3 (path "standard/DES-R1") |> color
  let r2 = Scad.import3 (path "standard/DES-R2") |> color
  let r3 = Scad.import3 (path "standard/DES-R3") |> color
  let r3_deep = Scad.import3 (path "standard/DES-R3-deep") |> color
  let r4 = Scad.import3 (path "standard/DES-R4") |> color
  let r5 = Scad.import3 (path "standard/DES-R5") |> color

  let kyria_thumb_r1t0 =
    Scad.import3 (path "thumb/DES-kyria-R1T0")
    |> Scad.rotate (v3 0. 0. (Float.pi /. 2.))
    |> color

  let kyria_thumb_r1t1 =
    Scad.import3 (path "thumb/DES-kyria-R1T1")
    |> Scad.rotate (v3 0. 0. (Float.pi /. 2.))
    |> color

  let kyria_thumb_r1t2 =
    Scad.import3 (path "thumb/DES-kyria-R1T2")
    |> Scad.rotate (v3 0. 0. (Float.pi /. 2.))
    |> color

  let kyria_thumb_r1t3 =
    Scad.import3 (path "thumb/DES-kyria-R1T3")
    |> Scad.rotate (v3 0. 0. (Float.pi /. 2.))
    |> color

  let row = function
    | 0 -> r4
    | 1 -> r3
    | 2 -> r2
    | _ -> r1

  let pseudo_scooped = function
    | 0 -> r1
    | 1 -> r5
    | 2 -> Scad.rotate (v3 0. 0. Float.pi) r2
    | _ -> r2

  let thumb = function
    | 0 -> kyria_thumb_r1t1
    | 1 -> kyria_thumb_r1t0
    | 2 -> kyria_thumb_r1t2
    | _ -> kyria_thumb_r1t3
end

module Chicago = struct
  (* Generated with https://github.com/pseudoku/PseudoMakeMeKeyCapProfiles *)
  let path n = path (Printf.sprintf "Pseudoku_Chicago/Chicago-%s.stl" n)
  let r2 = Scad.import3 (path "R2-R4") |> color
  let r4 = Scad.rotate (v3 0. 0. Float.pi) r2
  let r3_flat = Scad.import3 (path "R3-flat") |> color
  let r3_chord = Scad.import3 (path "R3-chord") |> color
  let thumb_1u = Scad.import3 (path "thumb-1u") |> color
  let thumb_1_5u = Scad.import3 (path "thumb-1_5u") |> color

  let row = function
    | 0 -> r2
    | 1 -> r3_flat
    | _ -> r4

  let thumb = function
    | 0 -> Scad.rotate (v3 0. 0. Float.pi) thumb_1u
    | _ -> thumb_1u
end
