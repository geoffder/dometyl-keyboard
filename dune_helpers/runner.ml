let () =
  let f i = ignore @@ Sys.command Sys.argv.(i + 1) in
  List.init (Array.length Sys.argv - 1) (Thread.create f) |> List.iter Thread.join
