open! Base
open! Scad_ml

let path n = Printf.sprintf "../things/caps/%s" n
let color = Model.color Color.DarkSlateBlue

module SA = struct
  let path n = path (Printf.sprintf "KeyV2_SA/SA-R%s.stl" n)
  let r0 = Model.import (path "0") |> color
  let r2 = Model.import (path "2") |> color
  let r3 = Model.import (path "3") |> color
  let r4 = Model.import (path "4") |> color
  let r5 = Model.import (path "5") |> color

  let row = function
    | 0 -> r4
    | 1 -> r3
    | 2 -> r2
    | _ -> r0

  let uniform _ = r3
end

module MT3 = struct
  let path n = path (Printf.sprintf "MT3/MT3-%s.stl" n)
  let space_1u = Model.import (path "1u-space") |> color
  let space_1_25u = Model.import (path "1.25u-space") |> color
  let space_1_5u = Model.import (path "1.5u-space") |> color
  let thumb_1u _ = space_1u
  let thumb_1_25u _ = space_1_25u
  let thumb_1_5u _ = space_1_5u
end

module Matty3 = struct
  let path n = path (Printf.sprintf "KeyV2_matty3/matty3-deep-R%s.stl" n)
  let r0 = Model.import (path "0") |> color
  let r1 = Model.import (path "1") |> color
  let r2 = Model.import (path "2") |> color
  let r3 = Model.import (path "3") |> color
  let r4 = Model.import (path "4") |> color
  let r5 = Model.import (path "5") |> color

  let row = function
    | 0 -> r4
    | 1 -> r3
    | 2 -> r2
    | _ -> r1
end

module MBK = struct
  let mbk = Model.import (path "MBK_darryldh_1u.stl") |> Model.color Color.DarkSlateBlue
  let row _ = mbk
end
