open Ocawklib

let read_file filepath = 
  In_channel.with_open_text filepath In_channel.input_all 
;;

let code = ref None
let input_file_paths = ref []

let vars = ref []

let usage_msg = "ocawk <code> <text_file> [<text_file2> ...]\nocawk -f <codefile> <text_file> [<text_file2> ...]" 
  

let speclist =
  let var_setter s =
    match String.split_on_char '=' s with
    | var :: tl -> vars := (var, String.concat "=" tl)::!vars
    | _ -> 
      Printf.eprintf "Incorrect value passed to -v \n";
      exit 1
  in
  [("-f", Arg.String (fun path -> code := Some (read_file path)), "Path to the file containg code");
   ("-F", Arg.String (fun sep -> vars := ("FS", sep)::!vars), "Set the field seperator (FS) variable");
   ("--field-seperator", Arg.String (fun sep -> vars := ("FS", sep)::!vars), "Set the field seperator (FS) variable");
   ("-v", Arg.String var_setter, "-v var=val\n Set the variable vat to value val") ]

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
    (* A file descriptor is passed to Run.run so that the evaluation is lazy *)
    let runner filepath env =
      let file = In_channel.open_text filepath in
      let v = Run.run env compiled_code file in
      In_channel.close file; v
    in 
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
  | e ->
    Printf.eprintf "Unhandled exception!\n";
    raise e
    (* exit 1 *)
    
  (* TODO: dodaj zamykanei file descriptorów *)