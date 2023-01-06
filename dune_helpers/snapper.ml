open OSCADml

let () =
  let f i =
    let n = Filename.(chop_extension @@ basename Sys.argv.(i + 1)) ^ ".png" in
    let f e = Printf.printf "Failed to take snapshot %s:\n%s\n%!" n e in
    Export.(
      snapshot
        ~colorscheme:TomorrowNight
        ~size:(1000, 1000)
        ~view:[ Axes; Scales ]
        n
        Sys.argv.(i + 1))
    |> Result.iter_error f
  in
  List.init (Array.length Sys.argv - 1) (Thread.create f) |> List.iter Thread.join
