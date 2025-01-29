open Values

open Values.EnvMonad


let default_env = VarMap.empty        |> 
    VarMap.add "FS"   (VString  " ")  |>
    VarMap.add "RS"   (VString "\n")  |> 
    VarMap.add "OFS"  (VString  " ")  |>
    VarMap.add "ORS"  (VString "\n")  |> 
    VarMap.add "NR"   (VNum 0.)       |>
    VarMap.add "FNR"  (VNum 0.)       |>
    VarMap.add "NF"   (VNum 0.)       |>
    VarMap.add "OFMT" (VString "%.6g")|> (* FIXME: Currently unused. add this to printing nums *)
    VarMap.add "FILENAME" (VString "") (* FIXME: Currently unused.*)
    (* TODO: this are not all: see https://www.gnu.org/software/gawk/manual/html_node/Built_002din-Variables.html *)

    
let get_value id env = ((lookup id |> view) env) |> snd
    
let remove_old_fields env = 
  let rec inner env i =
    if i = 0
    then env 
    else inner (VarMap.remove ("$"^string_of_int i) env) (i-1)  
  in
  inner env (get_value "NF" env |> float_of_value |> int_of_float)

let update_env (env: env) (line: string) =
  let fs = get_value "FS" env |> string_of_value in 
  let fields = Str.split (Str.regexp fs) line in
  let fields = if fields = [""] then [] else fields in
  let count = List.length fields in
  let env = remove_old_fields env in
  let env = env |>
    VarMap.add "NR" (VNum (1. +. float_of_value (get_value "NR" env))) |>
    VarMap.add "FNR" (VNum (1. +. float_of_value (get_value "FNR" env))) |>
    VarMap.add "NF" (VNum (float_of_int count)) |>
    VarMap.add "$0" (VString line)
    in
  let env, _ = List.fold_left (fun (env, i) field -> VarMap.add ("$" ^ string_of_int i) (VString field) env, i+1) (env, 1) fields in
  env


(* TODO refactor this to not use append, but a proper data structure *)
let take_until_sep (sep: string) (file: In_channel.t) : string =
  try
    let rec inner ending_buf =
      match In_channel.input_char file with
      | None -> sep
      | Some chr ->
        let chr = String.make 1 chr in
        let ending_buf = List.tl ending_buf @ [chr] in
        if String.concat "" ending_buf = sep
          then chr
          else chr ^ inner ending_buf
    in 
    let ending_buf = List.init (String.length sep) (fun _ -> input_char file |> String.make 1) in
    if String.concat "" ending_buf = sep
    then ""
    else
      let s = inner ending_buf in
      (String.concat "" ending_buf) ^ String.sub s 0 (String.length s - String.length sep)
  with 
  | End_of_file -> ""

(* Lazely find next line *)
let get_next_record (env: env) (file: In_channel.t) : string option  =
  let rs = get_value "RS" env |> string_of_value in
  let record = take_until_sep rs file in
  match record with
    | "" -> None
    | s -> Some s

let rec main_loop env (file: In_channel.t) script =
  match get_next_record env file with
  | None -> env
  | Some line ->
    let env = script (update_env env line) |> fst in
    main_loop env file script

  
let run_begin (env: env) (script: env -> env * unit): env =
  let env = VarMap.add "0#isBegin" (VBool true) env in
  let env = VarMap.add "0#isEnd" (VBool false) env in
  let env = fst (script env) in
  let env = VarMap.add "0#isBegin" (VBool false) env in
  env
  
let run_end (env: env) (script: env -> env * unit): env =
  let env = VarMap.add "0#isEnd" (VBool true) env in
  let env = fst (script env) in
  env

let run (env: env) (script: env -> env * unit) (file: In_channel.t): env =
  let env = main_loop env file script in
  env
  