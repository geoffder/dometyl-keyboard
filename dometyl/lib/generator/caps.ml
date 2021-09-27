open! Base
open! Scad_ml

let path n = Printf.sprintf "../things/caps/%s" n
let color = Scad.color Color.DarkSlateBlue

module SA = struct
  (* Generated with https://github.com/rsheldiii/KeyV2 *)
  let path n = path (Printf.sprintf "KeyV2_SA/SA-%s.stl" n)
  let r0 = Scad.import (path "R0") |> color
  let r2 = Scad.import (path "R2") |> color
  let r3 = Scad.import (path "R3") |> color
  let r4 = Scad.import (path "R4") |> color
  let r5 = Scad.import (path "R5") |> color

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
  let space_1u = Scad.import (path "1u-space") |> color
  let space_1_25u = Scad.import (path "1.25u-space") |> color
  let space_1_5u = Scad.import (path "1.5u-space") |> color
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
  let r0 = Scad.import (path "R0") |> color
  let r1 = Scad.import (path "R1") |> color
  let r2 = Scad.import (path "R2") |> color
  let r3 = Scad.import (path "R3") |> color
  let r4 = Scad.import (path "R4") |> color
  let r5 = Scad.import (path "R5") |> color

  let row = function
    | 0 -> r4
    | 1 -> r3
    | 2 -> r2
    | _ -> r1
end

module MBK = struct
  (* Reproduction by darryldh, found at https://www.thingiverse.com/thing:4564253 *)
  let mbk = Scad.import (path "darryldh_MBK/MBK_1u.stl") |> Scad.color Color.DarkSlateBlue
  let uniform _ = mbk
end

module Cherry = struct
  (* Generated with https://github.com/rsheldiii/KeyV2 *)
  let path n = path (Printf.sprintf "KeyV2_cherry/cherry-%s.stl" n)
  let r0 = Scad.import (path "R0") |> color
  let r2 = Scad.import (path "R2") |> color
  let r3 = Scad.import (path "R3") |> color
  let r4 = Scad.import (path "R4") |> color

  let row = function
    | 0 -> r4
    | 1 -> r3
    | 2 -> r2
    | _ -> r0
end

module OEM = struct
  (* Generated with https://github.com/rsheldiii/KeyV2 *)
  let path n = path (Printf.sprintf "KeyV2_OEM/OEM-%s.stl" n)
  let r1 = Scad.import (path "R1") |> color
  let r2 = Scad.import (path "R2") |> color
  let r3 = Scad.import (path "R3") |> color
  let r4 = Scad.import (path "R4") |> color
  let r5 = Scad.import (path "R5") |> color

  let row = function
    | 0 -> r4
    | 1 -> r3
    | 2 -> r2
    | 3 -> r1
    | _ -> r5
end

module DSA = struct
  (* Generated with https://github.com/rsheldiii/KeyV2 *)
  let r3 = Scad.import (path "KeyV2_DSA/DSA-R3.stl") |> color
  let uniform _ = r3
end

module DSS = struct
  (* Generated with https://github.com/rsheldiii/KeyV2 *)
  let path n = path (Printf.sprintf "KeyV2_DSS/DSS-%s.stl" n)
  let r1 = Scad.import (path "R1") |> color
  let r2 = Scad.import (path "R2") |> color
  let r3 = Scad.import (path "R3") |> color
  let r4 = Scad.import (path "R4") |> color
  let r5 = Scad.import (path "R5") |> color

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
  let r1 = Scad.import (path "standard/DES-R1") |> color
  let r2 = Scad.import (path "standard/DES-R2") |> color
  let r3 = Scad.import (path "standard/DES-R3") |> color
  let r3_deep = Scad.import (path "standard/DES-R3-deep") |> color
  let r4 = Scad.import (path "standard/DES-R4") |> color

  let keyria_thumb_r1t0 =
    Scad.import (path "thumb/DES-keyria-R1T0")
    |> Scad.rotate (0., 0., Float.pi /. 2.)
    |> color

  let keyria_thumb_r1t1 =
    Scad.import (path "thumb/DES-keyria-R1T1")
    |> Scad.rotate (0., 0., Float.pi /. 2.)
    |> color

  let keyria_thumb_r1t2 =
    Scad.import (path "thumb/DES-keyria-R1T2")
    |> Scad.rotate (0., 0., Float.pi /. 2.)
    |> color

  let keyria_thumb_r1t3 =
    Scad.import (path "thumb/DES-keyria-R1T3")
    |> Scad.rotate (0., 0., Float.pi /. 2.)
    |> color

  let row = function
    | 0 -> r4
    | 1 -> r3
    | 2 -> r2
    | _ -> r1

  let thumb = function
    | 0 -> keyria_thumb_r1t1
    | 1 -> keyria_thumb_r1t0
    | 2 -> keyria_thumb_r1t2
    | _ -> keyria_thumb_r1t3
end
