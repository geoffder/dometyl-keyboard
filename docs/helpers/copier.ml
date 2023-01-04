let sz = 8192
let bytes = Bytes.create sz

let copy src dest =
  let src = Unix.openfile src [ O_RDONLY ] 0
  and dest = Unix.openfile dest [ O_WRONLY; O_CREAT; O_TRUNC ] 0o777 in
  let rec loop () =
    match Unix.read src bytes 0 sz with
    | 0 -> ()
    | r ->
      ignore (Unix.write dest bytes 0 r);
      loop ()
  in
  loop ();
  Unix.close src;
  Unix.close dest

let () =
  let assets = Sys.argv.(1) in
  if not @@ Sys.file_exists assets then Sys.mkdir assets 0o777;
  for i = 2 to Array.length Sys.argv - 1 do
    let name = Filename.basename Sys.argv.(i) in
    ignore @@ copy Sys.argv.(i) (Printf.sprintf "%s/%s" assets name)
  done
