open Values

open Values.EnvMonad

let default_env =
    let value_env = 
      VarMap.empty        |> 
      VarMap.add "FS"   (VString  " ")      |>
      VarMap.add "RS"   (VString "\n")      |> 
      VarMap.add "OFS"  (VString  " ")      |>
      VarMap.add "ORS"  (VString "\n")      |> 
      VarMap.add "NR"   (VNum 0.)           |>
      VarMap.add "FNR"  (VNum 0.)           |>
      VarMap.add "NF"   (VNum 0.)           |>
      VarMap.add "CONVFMT" (VString "%.6g") |> (* FIXME: Currently unused. add this to converting nums to str *)
      VarMap.add "OFMT" (VString "%.6g")    |>
      VarMap.add "FILENAME" (VString "")
      (* TODO: these are not all: see https://www.gnu.org/software/gawk/manual/html_node/Built_002din-Variables.html *)
    in
    value_env, VarMap.empty

    

(* TODO: Move fields to use internal_value and make $ a operator *)
let remove_old_fields (env: env) : env = 
  let rec inner env i  : env =
    if i = 0
    then env 
    else inner (VarMap.remove ("$"^string_of_int i) (fst env), snd env) (i-1)  
  in
  inner env (get_value "NF" env |> float_of_value |> int_of_float)

let update_env (env: env) (line: string) : env =
  let fs = get_value "FS" env |> string_of_value in 
  let fields = Str.split (Str.regexp fs) line in
  let fields = if fields = [""] then [] else fields in
  let count = List.length fields in
  let env = remove_old_fields env in
  let val_env = (fst env) |>
    VarMap.add "NR" (VNum (1. +. float_of_value (get_value "NR" env))) |>
    VarMap.add "FNR" (VNum (1. +. float_of_value (get_value "FNR" env))) |>
    VarMap.add "NF" (VNum (float_of_int count)) |>
    VarMap.add "$0" (VString line)
    in
  let val_env, _ = List.fold_left (fun (val_env, i) field -> VarMap.add ("$" ^ string_of_int i) (VString field) val_env, i+1) (val_env, 1) fields in
  val_env, snd env
  
let take_until_sep (sep: string) (file: In_channel.t) : string option =
  let concat q = 
    Queue.fold (fun acc x -> acc^x) "" q
  in
  let rec inner (ending_buf: string Queue.t) =
    match In_channel.input_char file with
    | None -> sep
    | Some chr ->
      Queue.pop ending_buf |> ignore;
      let chr = String.make 1 chr in
      Queue.add chr ending_buf;
      if concat ending_buf = sep
        then chr
        else chr ^ inner ending_buf
  in
  try
    let seq = Seq.init (String.length sep) (fun _ -> input_char file |> String.make 1) in
    let ending_buf = Queue.of_seq seq in
    if concat ending_buf = sep
      then None
    else
      let prefix = concat ending_buf in
      let s = inner ending_buf in
      Some(String.sub (prefix ^ s) 0 (String.length s))
  with
    | End_of_file -> None
;;

(* Lazely find next line *)
let get_next_record (env: env) (file: In_channel.t) : string option  =
  let rs = get_value "RS" env |> string_of_value in
  take_until_sep rs file

let rec main_loop env (file: In_channel.t) (script: env -> env * unit) : env =
  match get_next_record env file with
  | None -> env
  | Some line ->
    let env = script (update_env env line) |> fst in
    main_loop env file script

  
let run_begin (env: env) (script: env -> env * unit): env =
  let env = fst @@ view (assign_internal "isEnd" (IVBool false)) env in
  let env = fst @@ view (assign_internal "isBegin" (IVBool true)) env in
  let env = fst (script env) in
  let env = fst @@ view( assign_internal "isBegin" (IVBool false)) env in
  env
  
let run_end (env: env) (script: env -> env * unit): env =
  let env = fst @@ view (assign_internal "isEnd" (IVBool true)) env in
  let env = fst (script env) in
  env

let run (env: env) (script: env -> env * unit) (file: In_channel.t) : env =
  let env = main_loop env file script in
  env

let run_repl (env: env) (script: env -> env * unit) : env =
  let sep = string_of_value @@ get_value "RS" env in
  let rec loop env xs = 
    match xs with
    | [] -> env, ""
    | [x; ""] ->
      let env = fst @@ script (update_env env x) in
      env, ""
    | [x] -> env, x
    | x::xs -> 
      let env = fst @@ script (update_env env x) in
      loop env xs
  in
  let rec inner env (ending: string) = 
    let line = read_line () in
    let records = Str.split_delim (Str.regexp sep) line in
    let records = match records with
      | [] -> [ending]
      | x::xs -> (ending ^ x) :: xs
    in
    let env, ending = loop env records in
    inner env ending
  in
  inner env ""