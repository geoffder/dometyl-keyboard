let sz = 8192
let bytes = Bytes.create sz

let copy src dest =
  let src = Unix.openfile src [ O_RDONLY ] 0
  and dest = Unix.openfile dest [ O_WRONLY; O_CREAT; O_TRUNC ] 0o666 in
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

let mkdirs root dirs =
  let rec loop path = function
    | [] -> path
    | hd :: tl ->
      let path = Filename.concat path hd in
      if Sys.file_exists path
      then loop path tl
      else begin
        Sys.mkdir path 0o777;
        loop path tl
      end
  in
  loop root dirs

let () =
  let dest = Sys.argv.(1) in
  Filename.(String.(split_on_char (get dir_sep 0) dest)) |> mkdirs "" |> ignore;
  for i = 3 to Array.length Sys.argv - 1 do
    let name = Filename.basename Sys.argv.(i) in
    let dir =
      (* HACK: indecisive on whether dropping the directories is always the way
           to go in this repo, so using this hard "flag" arg for now. *)
      if Sys.argv.(2) = "--basename"
      then dest
      else
        Filename.(String.(split_on_char (get dir_sep 0) (dirname Sys.argv.(i))))
        |> mkdirs dest
    in
    ignore @@ copy Sys.argv.(i) (Printf.sprintf "%s/%s" dir name)
  done
