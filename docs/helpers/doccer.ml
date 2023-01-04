type state =
  | Code
  | Doc

let digest src =
  let block = Buffer.create 256
  and out = Buffer.create 1024
  and ic = open_in src
  and oc =
    let name = Filename.(chop_extension @@ basename src) in
    open_out (name ^ ".mld")
  in
  let dump_block code =
    if code then Buffer.add_string out "\n{[";
    Buffer.add_buffer out block;
    if code then Buffer.add_string out "]}\n\n";
    Buffer.clear block
  in
  let doc_line s =
    let line s' =
      Buffer.add_string block s';
      Buffer.add_char block '\n'
    and trimmed =
      if String.starts_with ~prefix:"(**" s
      then String.(trim @@ sub s 3 (length s - 3))
      else s
    in
    if String.ends_with ~suffix:"*)" trimmed
    then (
      line String.(sub trimmed 0 (length trimmed - 2));
      dump_block false;
      None )
    else (
      line trimmed;
      Some Doc )
  in
  let rec loop state =
    match input_line ic with
    | line ->
      let trimmed = String.trim line in
      let line_state =
        if String.starts_with ~prefix:"(**" trimmed
        then Some Doc
        else if Option.is_none state && not (String.equal "" trimmed)
        then Some Code
        else None
      in
      let state' =
        match state, line_state with
        | Some Code, Some Doc ->
          dump_block true;
          doc_line trimmed
        | Some Doc, _ | None, Some Doc ->
          let state' = doc_line trimmed in
          if Option.is_none state' then dump_block false;
          state'
        | Some Code, (None | Some Code) | None, Some Code ->
          Buffer.add_char block '\n';
          Buffer.add_string block line;
          Some Code
        | None, None -> None
      in
      loop state'
    | exception End_of_file ->
      ( match state with
      | Some Code ->
        Buffer.add_char block '\n';
        dump_block true
      | Some Doc -> dump_block false
      | None -> () );
      Buffer.output_buffer oc out
  in
  loop None;
  close_in ic;
  close_out oc

let () =
  let oc = open_out "dune" in
  output_string oc "(documentation (package dometyl))";
  close_out oc;
  let f i = digest Sys.argv.(i + 1) in
  List.init (Array.length Sys.argv - 1) (Thread.create f) |> List.iter Thread.join
