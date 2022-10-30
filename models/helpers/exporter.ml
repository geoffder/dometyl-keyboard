open OSCADml

let () =
  let f i =
    let n = Filename.(chop_extension @@ basename Sys.argv.(i + 1)) ^ ".stl" in
    let f e = Printf.printf "Failed to export %s:\n%s\n%!" n e in
    Result.iter_error f (Export.script n Sys.argv.(i + 1))
  in
  List.init (Array.length Sys.argv - 1) (Thread.create f) |> List.iter Thread.join
