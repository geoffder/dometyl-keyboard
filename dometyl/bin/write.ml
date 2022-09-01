open Scad_ml

(* The relative path to the project root is given as an argument to the dometyl
   executable when it is run by the `run` rule (used with `dune build -w @run`
   for automated build/run on-save). *)
let path_to_root =
  try Sys.argv.(1) with
  | _ -> ""

let thing ?(export = false) name scad =
  let filename ext = Printf.sprintf "%s../things/%s.%s" path_to_root name ext in
  Printf.printf "- %s => scad" name;
  Out_channel.flush stdout;
  Scad.to_file (filename "scad") scad;
  if export
  then (
    match Export.script (filename "scad") (filename "stl") with
    | Ok () -> Printf.printf " => stl\n"
    | Error e -> failwith (Printf.sprintf " => export failed...\n%s\n" e) )
  else print_endline ""
