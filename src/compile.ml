open Ast
open Values

type compiled_stmt = env -> env * unit

let parse (s : string) : code =
  Parser.prog Lexer.read (Lexing.from_string s)

  
let regex_match text regex = 
  (try Str.search_forward (Str.regexp regex) text 0 with
    | Not_found -> -1) >= 0

  
let eval_binop bop v1 v2 =
  match bop, v1, v2 with
  | Mult, v1, v2 -> VNum (float_of_value v1 *. float_of_value v2)
  | Div,  v1, v2 -> VNum (float_of_value v1 /. float_of_value v2)
  | Add,  v1, v2 -> VNum (float_of_value v1 +. float_of_value v2)
  | Sub,  v1, v2 -> VNum (float_of_value v1 -. float_of_value v2)
  | Eq,   v1, v2 -> VBool (string_of_value v1 = string_of_value v2)
  | Lt,   VNum v1,  VNum v2 -> VBool (v1 < v2) 
  | Lt,   v1, v2 -> VBool (string_of_value v1 < string_of_value v2)
  | Le,   VNum v1,  VNum v2 -> VBool (v1 <= v2)
  | Le,   v1, v2 -> VBool (string_of_value v1 <= string_of_value v2)
  | Gt,   VNum v1,  VNum v2 -> VBool (v1 > v2)
  | Gt,   v1, v2 -> VBool (string_of_value v1 > string_of_value v2)
  | Ge,   VNum v1,  VNum v2 -> VBool (v1 >= v2)
  | Ge,   v1, v2 -> VBool (string_of_value v1 >= string_of_value v2)
  | Neq,  VNum v1,  VNum v2 -> VBool (v1 != v2)
  | Neq,  v1, v2 -> VBool (string_of_value v1 != string_of_value v2)
  | Land, v1, v2 -> VBool (bool_of_value v1 && bool_of_value v2)
  | Lor,  v1, v2 -> VBool (bool_of_value v1 || bool_of_value v2)
  | RegMatch, v1, v2 ->
    let matched = string_of_value v1 in
    VBool (regex_match matched (string_of_value v2))
  | NRegMatch, v1, v2 ->
      let matched = string_of_value v1 in
      VBool (regex_match matched (string_of_value v2))
  
let rec eval_expr (e: expr) (env: env) : env * value = 
  match e with
  | Num n  -> env, VNum n
  | Str s  -> env, VString s
  | Var idnt -> env, lookup idnt env
  | Binop(bop, e1, e2) ->
    let env, v1 = eval_expr e1 env in
    let env, v2 = eval_expr e2 env in
    env, eval_binop bop v1 v2
  | Assign(id, e) ->
    let env, v = eval_expr e env in
    let env = VarMap.add id v env in
    env, v 
    
    
  
let eval_trigger (cond: condition) (env: env): bool =
  match cond with
  | Regex r -> regex_match (lookup "$0" env |> string_of_value) r    
  | Expr e  -> eval_expr e env |> snd |> bool_of_value (* Ignore enviroment modification in trigger evaluation *)
  | Begin   -> lookup "$isBegin" env |> bool_of_value
  | End     -> lookup "$isEnd" env |> bool_of_value
;;


let eval_print (env:env) (exprs: expr list) = 
  (* evaluate all the expressions, store them and print them. The value of OFS
  is whatever it is after ALL the evaluations (got this behaviour by testing awk) *)
  let rec inner env exprs = 
    match exprs with
    | [] -> env, []
    | e::es -> 
      let env, v = eval_expr e env in
      let env, tail = inner env es in
      env, (string_of_value v) :: tail
  in
  let env, elems = if exprs = []
    then env, [lookup "$0" env |> string_of_value]
    else inner env exprs
  in 
  let ofs = lookup "OFS" env |> string_of_value in
  let ors = lookup "ORS" env |> string_of_value in
  let line = String.concat ofs elems in
  print_string line;
  print_string ors;
  env, ()
  
let rec eval_stmt (stmt: stmt) (env: env) : env * unit =
  match stmt with
  | Print es -> eval_print env es
  | If(p, t, f) ->
    let env, v = eval_expr p env in
    if v |> bool_of_value
    then eval_actions t env
    else eval_actions f env
  | ExprStmt e -> 
    let env, _ = eval_expr e env in env, ()
    
    
and eval_actions (actions: stmt list) (env: env) : env * unit =
  List.fold_left (fun env stmt -> eval_stmt stmt env |> fst) env actions, ()

let eval_instruction (instr: instruction): compiled_stmt = 
  let triggers, actions = instr in 
  fun env -> 
    if (triggers = [] && not (lookup "$isBegin" env |> bool_of_value) && not (lookup "$isEnd" env |> bool_of_value)) || List.exists (fun x -> eval_trigger x env) triggers 
    then 
      eval_actions actions env
    else env, ()
  
let compile_code (code: code): compiled_stmt = fun env -> 
  List.fold_left (fun acc instr -> (eval_instruction instr) acc |> fst) env code, ()

let compile (s : string)  =
   s |> parse |> compile_code
;;

