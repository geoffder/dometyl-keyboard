open! Base
open! Scad_ml

let path n = Printf.sprintf "../things/caps/%s" n
let sa_r3 = Model.import (path "SA-R3.stl") |> Model.color Color.DarkSlateBlue
let mbk = Model.import (path "MBK_darryldh_1u.stl") |> Model.color Color.DarkSlateBlue
