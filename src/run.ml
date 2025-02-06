open Values
open Values.EnvMonad


let default_env = Defaults.value_env, Defaults.internal_env

(* General utility function *)
let get_value ident env = ((EnvMonad.lookup ident |> EnvMonad.view) env) |> snd;;

let remove_old_fields (env: env) : env = 
  let rec inner env i  : env =
    if i = 0
    then env 
    else inner (fst env, StrMap.remove ("$"^string_of_int i) (snd env)) (i-1)  
  in
  inner env (get_value "NF" env |> float_of_value |> int_of_float)

let update_env (env: env) (line: string) : env =
  let fs = get_value "FS" env |> string_of_value in 
  let fields = Str.split (Str.regexp fs) line in
  let count = List.length fields in
  let val_env, in_env = remove_old_fields env in
  let val_env = val_env |>
    StrMap.add "NR" (VNum (1. +. float_of_value (get_value "NR" env))) |>
    StrMap.add "FNR" (VNum (1. +. float_of_value (get_value "FNR" env))) |>
    StrMap.add "NF" (VNum (float_of_int count))
  in
  let in_env = in_env |>
    StrMap.add "$0" (IVString line)
  in
  let in_env, _ = List.fold_left (fun (in_env, i) field -> StrMap.add ("$" ^ string_of_int i) (IVString field) in_env, i+1) (in_env, 1) fields in
  val_env, in_env
  
(* Lazely find next line *)
let get_next_record (env: env) (file: In_channel.t) : string option  =
  let sep = get_value "RS" env |> string_of_value in
  let concat q = 
    Queue.fold (fun acc x -> acc^x) "" q
  in
  let rec inner (ending_buf: string Queue.t) acc =
    match In_channel.input_char file with
    | None -> acc^sep
    | Some chr ->
      Queue.pop ending_buf |> ignore;
      let chr = String.make 1 chr in
      Queue.add chr ending_buf;
      if concat ending_buf = sep
        then acc^chr
        else inner ending_buf (acc^chr)
  in
  try
    let seq = Seq.init (String.length sep) (fun _ -> input_char file |> String.make 1) in
    let ending_buf = Queue.of_seq seq in
    if concat ending_buf = sep
      then None
    else
      let prefix = concat ending_buf in
      let s = inner ending_buf "" in
      Some(String.sub (prefix ^ s) 0 (String.length s))
  with
    | End_of_file -> None
;;

let rec main_loop env (script: env -> env * unit) (file: In_channel.t) : env =
  match get_next_record env file with
  | None -> env
  | Some line ->
    let env = fst @@ script (update_env env line) in
    main_loop env script file

  
let run_begin (env: env) (script: env -> env * unit): env =
  let v_env, i_env = env in
  let i_env = StrMap.add "isEnd" (IVBool false) i_env in
  let i_env = StrMap.add "isBegin" (IVBool true) i_env in
  let v_env, i_env = fst @@ script (v_env, i_env) in
  let i_env = StrMap.add "isBegin" (IVBool false) i_env in
  v_env, i_env
  
let run_end (env: env) (script: env -> env * unit): env =
  let v_env, i_env = env in
  let i_env = StrMap.add "isEnd" (IVBool true) i_env in
  let v_env, i_env = fst @@ script (v_env, i_env) in
  v_env, i_env

let run = main_loop

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
    let line = line ^ "\n" in
    let records = Str.split_delim (Str.regexp sep) line in
    let records = match records with
      | [] -> [ending]
      | x::xs -> (ending ^ x) :: xs
    in
    let env, ending = loop env records in
    inner env ending
  in
  inner env ""