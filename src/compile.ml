open Ast
open Values
open Exceptions
open Values.EnvMonad

type compiled_stmt = env -> env * unit



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
  | Eq,   v1, v2 -> VNum (float_of_bool (string_of_value v1 = string_of_value v2))
  | Lt,   VNum v1,  VNum v2 -> VNum (float_of_bool (v1 < v2) )
  | Lt,   v1, v2 -> VNum (float_of_bool (string_of_value v1 < string_of_value v2))
  | Le,   VNum v1,  VNum v2 -> VNum (float_of_bool (v1 <= v2))
  | Le,   v1, v2 -> VNum (float_of_bool (string_of_value v1 <= string_of_value v2))
  | Gt,   VNum v1,  VNum v2 -> VNum (float_of_bool (v1 > v2))
  | Gt,   v1, v2 -> VNum (float_of_bool (string_of_value v1 > string_of_value v2))
  | Ge,   VNum v1,  VNum v2 -> VNum (float_of_bool (v1 >= v2))
  | Ge,   v1, v2 -> VNum (float_of_bool (string_of_value v1 >= string_of_value v2))
  | Neq,  VNum v1,  VNum v2 -> VNum (float_of_bool (v1 <> v2))
  | Neq,  v1, v2 -> VNum (float_of_bool (string_of_value v1 <> string_of_value v2))
  | Land, v1, v2 -> VNum (float_of_bool (bool_of_value v1 && bool_of_value v2))
  | Lor,  v1, v2 -> VNum (float_of_bool (bool_of_value v1 || bool_of_value v2))
  | RegMatch, v1, v2 ->
    let matched = string_of_value v1 in
    VNum (float_of_bool (regex_match matched (string_of_value v2)))
  | NRegMatch, v1, v2 ->
      let matched = string_of_value v1 in
      VNum (float_of_bool (regex_match matched (string_of_value v2)))
  | Concat, v1, v2 -> VString (string_of_value v1 ^ string_of_value v2)

      
let rec rebuild_line (sep: string) (n: int) = 
  if n = 1
  then
    let* f = lookup_internal "$1" in
    f |> string_of_internal_value_option |> return
  else
    let* field = lookup_internal ("$" ^ string_of_int n) in
    let field = string_of_internal_value_option field in
    let* rest = rebuild_line sep (n-1) in
    return (rest ^ sep ^ field)

let rec rebuild_fields (fields: string list) (i: int) =
  match fields with
  | [] -> failwith "this shouldn't happen (Compile.rebuild_fields)"
  | [x] -> assign_internal ("$" ^ string_of_int i) (IVString x)
  | x::xs ->
    assign_internal ("$" ^ string_of_int i) (IVString x) >>
    rebuild_fields xs (i+1)
    
let rebuild_record n (line: string) =
  match n with
  | 0 ->
    let* fs = lookup "FS" in
    let fs = string_of_value fs in
    let fields = Str.split (Str.regexp fs) line in
    let fields = if fields = [""] then [] else fields in
    let count = List.length fields in
    assign "NF" (VNum (float_of_int count)) >>
    rebuild_fields fields 1
  | n ->
    let* nf = lookup "NF" in
    let* sep = lookup "OFS" in
    let n = max n (nf |> float_of_value |> int_of_float) in
    let* line = rebuild_line (string_of_value sep) n in
    assign_internal "$0" (IVString line)

    
let floatless_string_of_float f = 
  if Float.is_integer f
  then string_of_int @@ int_of_float f
  else string_of_float f 
      
let rec eval_expr (e: expr ) : value t = 
   match e with
  | Num n  -> VNum n |> return
  | Str s  -> VString s |> return
  | VarE (Var ident) -> lookup ident
  | VarE (FieldRef e) ->
    let* n = eval_expr e in
    let* res = lookup_internal ("$" ^ string_of_value n) in
    return @@ VString (string_of_internal_value_option res)
  | Binop(bop, e1, e2) ->
    let* v1 = eval_expr e1 in
    let* v2 = eval_expr e2 in
    eval_binop bop v1 v2 |> return
  | Assign(Var id, e) ->
    let* v = eval_expr e in
    let* v = assign id v in
    return v
  | PreInc (Var id) -> 
    let* v = lookup id in
    assign id  (VNum ( float_of_value v +. 1.))
  | PostInc (Var id) -> 
    let* v = lookup id in
    assign id  (VNum ( float_of_value v +. 1.)) >>
    return v
  | PreDec (Var id) -> 
    let* v = lookup id in
    assign id  (VNum ( float_of_value v -. 1.))
  | PostDec (Var id) ->
    let* v = lookup id in
    assign id  (VNum ( float_of_value v -. 1.)) >>
    return v
  | Assign(FieldRef e1, e2) -> (* FIXME: Refactor -- move assign and return into rebuild_record? *)
    let* v = eval_expr e2 in (* NOTE: First evaluate the value, then the ident -- this is what awk does *)
    let v = string_of_value v in
    let* n = eval_expr e1 in
    if (float_of_value n) < 0. then raise @@ Exceptions.AccessError ("Attempt to access field " ^ (string_of_value n))
    else 
    assign_internal ("$" ^ string_of_value n) (IVString v) >>
    rebuild_record (int_of_float @@ float_of_value n) v >>
    return (VString v)
  | PreInc (FieldRef e) -> 
    let* n = eval_expr e in
    if (float_of_value n) < 0. then raise @@ Exceptions.AccessError ("Attempt to access field " ^ (string_of_value n))
    else 
    let* v = lookup_internal ("$" ^ string_of_value n) in
    let  v = float_of_internal_value_option v in
    assign_internal ("$" ^ string_of_value n) (IVString (floatless_string_of_float (v +. 1.))) >>
    rebuild_record (int_of_float @@ float_of_value n) (floatless_string_of_float (v +. 1.)) >>
    return (VString  (floatless_string_of_float (v +. 1.)))
  | PostInc (FieldRef e) -> 
    let* n = eval_expr e in
    if (float_of_value n) < 0. then raise @@ Exceptions.InternalValueError ("Attempt to access field " ^ (string_of_value n))
    else 
    let* v = lookup_internal ("$" ^ string_of_value n) in
    let  v = float_of_internal_value_option v in
    assign_internal ("$" ^ string_of_value n) (IVString (floatless_string_of_float (v +. 1.))) >>
    rebuild_record (int_of_float @@ float_of_value n) (floatless_string_of_float (v +. 1.)) >>
    return (VString  (floatless_string_of_float v))
  | PreDec (FieldRef e) -> 
    let* n = eval_expr e in
    if (float_of_value n) < 0. then raise @@ Exceptions.AccessError ("Attempt to access field " ^ (string_of_value n))
    else 
    let* v = lookup_internal ("$" ^ string_of_value n) in
    let  v = float_of_internal_value_option v in
    assign_internal ("$" ^ string_of_value n) (IVString (floatless_string_of_float (v -. 1.))) >>
    rebuild_record (int_of_float @@ float_of_value n) (floatless_string_of_float (v -. 1.)) >>
    return (VString  (floatless_string_of_float (v -. 1.)))
  | PostDec (FieldRef e) ->
    let* n = eval_expr e in
    if (float_of_value n) < 0. then raise @@ Exceptions.AccessError ("Attempt to access field " ^ (string_of_value n))
    else 
    let* v = lookup_internal ("$" ^ string_of_value n) in
    let  v = float_of_internal_value_option v in
    assign_internal ("$" ^ string_of_value n) (IVString (floatless_string_of_float (v -. 1.))) >>
    rebuild_record (int_of_float @@ float_of_value n) (floatless_string_of_float (v -. 1.)) >>
    return (VString  (floatless_string_of_float v))
  | FunctionCall(id, es) ->
    let* values = eval_expr_list es in
    let* func = lookup_internal @@ "func_" ^ id in
    (func_of_internal_value_option func) values
    
and eval_expr_list es =
  match es with
  | [] -> return []
  | x::xs ->
    let* v = eval_expr x in
    let* values = eval_expr_list xs in
    return (v :: values)
  
let eval_trigger (cond: condition) : bool t =
  match cond with
  | Always ->
    let* p1 = lookup_internal "isBegin" in
    let* p2 = lookup_internal "isEnd" in 
    return @@ not (bool_of_internal_value_option p1 || bool_of_internal_value_option p2)
  | Regex r -> 
    let* record = lookup_internal "$0" in
    let v = regex_match (string_of_internal_value_option record) r in
    let* p1 = lookup_internal "isBegin" in
    let* p2 = lookup_internal "isEnd" in 
    (v && (p1 |> bool_of_internal_value_option |> not) && (p2 |> bool_of_internal_value_option |> not)) |> return 
  | Expr e  -> 
    let* p1 = lookup_internal "isBegin" in
    let* p2 = lookup_internal "isEnd" in
    (* If begin or end dont evaluate v *)
    if (p1 |> bool_of_internal_value_option) || (p2 |> bool_of_internal_value_option)
    then
      return false
    else
      let* v = eval_expr e in 
      v |> bool_of_value |> return
  | Begin   -> 
    let* v = lookup_internal "isBegin" in v |> bool_of_internal_value_option |> return
  | End     -> 
    let* v = lookup_internal "isEnd" in v |> bool_of_internal_value_option |> return
;;


let eval_print (exprs: expr list): string t = 
  (* evaluate all the expressions, store them and print them. The value of OFS
  is whatever it is after ALL the evaluations (got this behaviour by testing awk) *)
  let rec inner (exprs: expr list) : string list t = 
    match exprs with
    | [] -> return []
    | e::es -> 
      let* ofmt = lookup "OFMT" in
      let* v = eval_expr e in
      let v = match v with
        | VString s -> s
        | VNum n -> 
          let ofmt = string_of_value ofmt in
          Printf.sprintf (Scanf.format_from_string ofmt "%.6g") n
      in
      let* tail = inner es in
      return (v :: tail)
  in
  let* elems = if exprs = []
    then
      let* elem = lookup_internal "$0" in
      return [elem |> string_of_internal_value_option]
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
  let* file_descriptor = lookup_internal @@ "file_" ^ file_name in
  let file_descriptor = match file_descriptor with
    | Some (IVFileDescriptor fd) -> fd
    | Some _ -> raise (InternalValueError file_name)
    | None -> Out_channel.open_gen flags 0o666 file_name
  in
  output_string file_descriptor (line ^ ors);
  assign_internal ("file_" ^ file_name) (IVFileDescriptor file_descriptor) >>
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
    then eval_action t
    else eval_action f
  | ExprStmt e -> 
    eval_expr e >> return ()
  | PrintWrite(es, e) ->
    let flags = [Out_channel.Open_wronly; Out_channel.Open_creat; Out_channel.Open_trunc; Open_text] in
    print_to_file e es flags
  | PrintAppend(es, e) ->
    let flags = [Out_channel.Open_append; Out_channel.Open_creat; Open_text] in
    print_to_file e es flags
  | For(init, cond, incr, body) ->
    eval_expr init >>
    eval_for cond incr body
  | While(cond, body) -> eval_while cond body
  | DoWhile(body, cond) ->
    eval_action body >>
    eval_while cond body
  
and eval_action (actions: stmt list) : unit t =
  match actions with
  | [] -> return ()
  | x::xs ->
    eval_stmt x >>
    eval_action xs

and eval_for (cond: expr) (incr: expr) (body: stmt list) : unit t = 
  let* p = eval_expr cond in
  if bool_of_value p then
    eval_action body >>
    eval_expr incr >>
    eval_for cond incr body
  else
    return ()
    
and eval_while (cond: expr) (body: stmt list) : unit t = 
  let* p = eval_expr cond in
  if bool_of_value p then
    eval_action body >>
    eval_while cond body
  else
    return ()
  
  
    
let eval_instruction (instr: instruction): unit t = 
  let trigger, actions = instr in
  let* is_trigged = eval_trigger trigger in
  if is_trigged
  then 
    eval_action actions
  else return ()
  
let compile_code (code: code): compiled_stmt = fun env -> 
  List.fold_left (fun acc instr -> (eval_instruction instr |> view) acc |> fst) env code, ()

let compile (s : string)  =
   s |> parse |> compile_code
;;

