open Ocawklib



let read_file filepath = 
  In_channel.with_open_bin filepath In_channel.input_all 
;;

let code = ref None
let input_file_paths = ref []

let vars = ref []

let usage_msg = "ocawk <code> <text_file> [<text_file2> ...]\nocawk -f <codefile> <text_file> [<text_file2> ...]" 
  
let speclist =
  [("-f", Arg.String (fun path -> code := Some (read_file path)), "Path to the file containg code");
   ("-F", Arg.String (fun sep -> vars := ("FS", sep)::!vars), "Set the field seperator (FS) variable");
   ("--field-seperator", Arg.String (fun sep -> vars := ("FS", sep)::!vars), "Set the field seperator (FS) variable");
   (* ("-v", Arg.String (fun sep -> vars := ("FS", sep)::!vars), "-v var=val\n Set the variable vat to value val") *)] (* TODO *) 

let anon_fun arg = 
  match !code with
  | None -> 
      code := Some arg
  | Some _ ->
    input_file_paths := arg::!input_file_paths


let () =
  Arg.parse speclist anon_fun usage_msg;
  try
    let compiled_code =
      match !code with
      | None ->
        Printf.eprintf "No code provided\n";
        exit 1
      | Some c -> Compile.compile c
    in
    let setter = (fun env (var, val_) -> Values.VarMap.add var (Values.VString val_) env ) in
    let env = List.fold_left setter Run.default_env !vars in
    let env = Run.run_begin env compiled_code in
    (* TODO: instead of reading a full file make Run.run take the in_channel file descriptor and read as much as it needs 
    this gives lazyness, and we like lazyness *)
    let runner = (fun filepath env -> Run.run env compiled_code (read_file filepath)) in 
    let env = List.fold_right runner !input_file_paths env in
    let env = Run.run_end env compiled_code in 
    ignore env
  with
  | Compile.Parse_error(pos, tok) ->
    Printf.eprintf "%s:%d:%d: Syntax error, unexpected token %s\n"
      pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)
      tok;
    Printf.eprintf "Try running 'echo <your code> to see if bash doesn't substitute variable names\n";
      Printf.eprintf "Try using \' instead of \"\n";
    exit 1
  | Parser.Error -> 
    Printf.eprintf "Unhandled parsing error!\n";
    exit 1
  | _ ->
    Printf.eprintf "Unhandled exception!\n";
    exit 1