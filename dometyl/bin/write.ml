open! Base
open! Scad_ml

(* The relative path to the project root is given as an argument to the dometyl
   executable when it is run by the `run` rule (used with `dune build -w @run`
   for automated build/run on-save). *)
let path_to_root =
  try (Sys.get_argv ()).(1) with
  | _ -> ""

let thing ?(export = false) name scad =
  let filename ext = Printf.sprintf "%s../things/%s.%s" path_to_root name ext in
  Stdio.printf "- %s => scad" name;
  Stdio.(Out_channel.flush stdout);
  let oc = Stdio.Out_channel.create (filename "scad") in
  Scad.write oc scad;
  if export
  then (
    Stdio.printf " => stl\n";
    Stdio.(Out_channel.flush stdout);
    try
      Printf.sprintf
        ( if Sys.unix
        then "openscad -q -o %s --export-format binstl %s"
        else "openscad.com -q -o %s --export-format binstl %s" )
        (filename "stl")
        (filename "scad")
      |> Caml.Sys.command
      |> function
      | 0 -> ()
      | _ -> failwith ""
    with
    | _ -> Stdio.print_endline "Openscad export shell command failed." )
  else Stdio.print_endline ""
