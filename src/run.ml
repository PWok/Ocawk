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

let remove_old_fields env = 
  let rec inner env i =
    if i = 0
    then env 
    else inner (VarMap.remove ("$"^string_of_int i) env) (i-1)  
  in
  inner env (((lookup "NF" |> view) env) |> snd |> float_of_value |> int_of_float) (* TODO: Rewrite this to properly use the monad not view  *)

let update_env (env: env) (line: string) =
  let fs = (lookup "FS" |> view) env |> snd |> string_of_value in 
  let fields = Str.split (Str.regexp fs) line in
  let fields = if fields = [""] then [] else fields in
  let count = List.length fields in
  let env = remove_old_fields env in
  let env = env |>
    VarMap.add "NR" (VNum (1. +. float_of_value ((lookup "NR" |> view) env |> snd ) )) |>
    VarMap.add "FNR" (VNum (1. +. float_of_value ((lookup "FNR" |> view) env |> snd ))) |>
    VarMap.add "NF" (VNum (float_of_int count)) |>
    VarMap.add "$0" (VString line)
    in
  let env, _ = List.fold_left (fun (env, i) field -> VarMap.add ("$" ^ string_of_int i) (VString field) env, i+1) (env, 1) fields in
  env

  
(* FIXME: get rid of all the view function uses *)
  
let get_next_line (env: env) (text: string) : string option * string option  =
  let rs = (lookup "RS" |> view) env |> snd |> string_of_value in
  let split = Str.bounded_split (Str.regexp rs) text 2 in
  match split with
    | [] -> None, None
    | [line] -> Some line, None
    | line :: [rest] -> Some line, Some rest
    | _ -> failwith "this shouldn't be possible (get_next_line)" 

let rec main_loop env lines script =
  match get_next_line env lines with
  | None, None -> env
  | Some line, None -> 
    let env = script (update_env env line) |> fst in
    env
  | Some line, Some tail ->
    let env = script (update_env env line) |> fst in
    main_loop env tail script
  | _ -> failwith "this shouldn't be possible (main loop)" 

  
let run_begin (env: env) (script: env -> env * unit): env =
  let env = VarMap.add "$isBegin" (VBool true) env in
  let env = VarMap.add "$isEnd" (VBool false) env in
  let env = fst (script env) in
  let env = VarMap.add "$isBegin" (VBool false) env in
  env
  
let run_end (env: env) (script: env -> env * unit): env =
  let env = VarMap.add "$isEnd" (VBool true) env in
  let env = fst (script env) in
  env

let run (env: env) (script: env -> env * unit) (input_text: string): env =
  let env = main_loop env input_text script in
  env
  