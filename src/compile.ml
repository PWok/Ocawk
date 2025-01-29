open Ast
open Values


open Values.EnvMonad
let (let*) = EnvMonad.bind


type compiled_stmt = env -> env * unit

exception Parse_error of Lexing.position * string

let parse (s : string) : code =
  let lexbuf = Lexing.from_string s in
  Lexing.set_filename lexbuf "<string>";
  try Parser.prog Lexer.read lexbuf with
  | Parser.Error ->
    raise (Parse_error(Lexing.lexeme_start_p lexbuf, Lexing.lexeme lexbuf))

  
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
  | Concat, v1, v2 -> VString (string_of_value v1 ^ string_of_value v2)

      
let rec rebuild_line (sep: string) (n: int) = 
  if n = 1
  then
    let* f = lookup "$1" in
    f |> string_of_value |> return
  else
    let* field = lookup ("$" ^ string_of_int n) in
    let field = string_of_value field in
    let* rest = rebuild_line sep (n-1) in
    return (rest ^ sep ^ field)

let rec rebuild_fields (fields: string list) (i: int) : value t =
  match fields with
  | [] -> failwith "this shouldn't happen (Compile.rebuild_fields)"
  | [x] -> assign ("$" ^ string_of_int i) (VString x)
  | x::xs ->
    let* _ = assign ("$" ^ string_of_int i) (VString x) in
    rebuild_fields xs (i+1)
    
let rebuild_record id (v: value) : value t =
  let n = int_of_string_opt (String.sub id 1 ((String.length id) - 1)) in
  match n with
  | None -> return v
  | Some 0 ->
    let* fs = lookup "FS" in
    let fs = string_of_value fs in
    let line = string_of_value v in
    let fields = Str.split (Str.regexp fs) line in
    let fields = if fields = [""] then [] else fields in
    let count = List.length fields in
    let* m = assign "NF" (VNum (float_of_int count)) in
    if fields != []
      then rebuild_fields fields 1
      else return m
  | Some n ->
    let* nf = lookup "NF" in
    let* sep = lookup "OFS" in
    let n = max n (nf |> float_of_value |> int_of_float) in
    let* line = rebuild_line (string_of_value sep) n in
    assign "$0" (VString line)
    
    
      
let rec eval_expr (e: expr ) : value t = 
   match e with
  | Num n  -> VNum n |> return
  | Str s  -> VString s |> return
  | Var ident -> lookup ident
  | Binop(bop, e1, e2) ->
    let* v1 = eval_expr e1 in
    let* v2 = eval_expr e2 in
    eval_binop bop v1 v2 |> return
  | Assign(id, e) ->
    let* v = eval_expr e in
    let* v = assign id v in
    if '$' = String.get id 0 (* TODO: if $0 is set the nrebuild all fields*)
      then rebuild_record id v
      else return v
    
    
  
let eval_trigger (cond: condition) : bool t =
  match cond with
  | Regex r -> 
    let* record = lookup "$0" in
    regex_match (string_of_value record) r |> return
  | Expr e  -> 
    let* v = eval_expr e in 
    v |> bool_of_value |> return (* Ignore enviroment modification in trigger evaluation *)
  | Begin   -> 
    let* v = lookup "0#isBegin" in v |> bool_of_value |> return
  | End     -> 
    let* v = lookup "0#isEnd" in v |> bool_of_value |> return
;;


let eval_print (exprs: expr list): _ t = 
  (* evaluate all the expressions, store them and print them. The value of OFS
  is whatever it is after ALL the evaluations (got this behaviour by testing awk) *)
  let rec inner (exprs: expr list) : string list t = 
    match exprs with
    | [] -> return []
    | e::es -> 
      let* v = eval_expr e in
      let* tail = inner es in
      return ((string_of_value v) :: tail)
  in
  let* elems = if exprs = []
    then
      let* elem = lookup "$0" in
      return [elem |> string_of_value]
    else inner exprs
  in 
  let* ofs = lookup "OFS" in
  let line = String.concat (string_of_value ofs) elems in
  return line

  
let print_to_file e es flags = 
  let* file_name = eval_expr e in
  let file_name = string_of_value file_name in
  let* line = eval_print es in
  let* ors = lookup "ORS"  in
  let ors = string_of_value ors in
  let* file_descriptor = lookup @@ "0#" ^ file_name in
  let file_descriptor = if is_filedesc file_descriptor
    then filedesc_of_value file_descriptor
    else Out_channel.open_gen flags 0o666 file_name
  in
  output_string file_descriptor (line ^ ors);
  let* _ = assign ("0#" ^ file_name) (VFileDesc file_descriptor) in
  return ()
  
let rec eval_stmt (stmt: stmt) : unit t =
  match stmt with
  | Print es ->
    let* line = eval_print es in
    let* ors = lookup "ORS"  in
    print_string line;
    print_string @@ string_of_value ors;
    return ()
  | If(p, t, f) ->
    let* v = eval_expr p in
    if v |> bool_of_value
    then eval_actions t
    else eval_actions f
  | ExprStmt e -> 
    let* _ = eval_expr e in return ()
  | PrintWrite(es, e) ->
    let flags = [Out_channel.Open_wronly; Out_channel.Open_creat; Out_channel.Open_trunc; Open_text] in
    print_to_file e es flags
  | PrintAppend(es, e) ->
    let flags = [Out_channel.Open_append; Out_channel.Open_creat; Open_text] in
    print_to_file e es flags
    
and eval_actions (actions: stmt list) : unit t =
  match actions with
  | [] -> return ()
  | x::xs ->
    let* _ = eval_stmt x in
    eval_actions xs

    
let rec check_triggers (triggers: condition list) : bool t =
  match triggers with
  | [] -> return false
  | x :: xs -> 
    let* v = eval_trigger x in
    if v
      then return true
      else check_triggers xs 
    
let eval_instruction (instr: instruction): unit t = 
  let triggers, actions = instr in 
  let* is_begin = lookup "0#isBegin" in
  let* is_end = lookup "0#isEnd" in
  let* is_trigged = check_triggers triggers in
  if (triggers = [] && not (is_begin |> bool_of_value) && not (is_end |> bool_of_value)) || is_trigged
  then 
    eval_actions actions
  else return ()
  
let compile_code (code: code): compiled_stmt = fun env -> 
  List.fold_left (fun acc instr -> (eval_instruction instr |> view) acc |> fst) env code, ()

let compile (s : string)  =
   s |> parse |> compile_code
;;

