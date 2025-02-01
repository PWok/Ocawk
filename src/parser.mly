%{
open Ast

type print_type = 
  | Normal
  | Write of expr
  | Append of expr
  
let devar v = 
  match v with
  | Var x -> x
  | _ -> failwith "this shouldn't (parser devar). Check your code for syntax errors (Increment/Decrement/function call of not identifiers)"

%}


%token <float> NUM
%token <string> IDENT
%token <string> STR
%token <string> REGEX

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE

%token INCREMENT
%token DECREMENT

%token TIMES
%token DIV
%token PLUS
%token MINUS

%token AND
%token OR

%token EQ
%token LT
%token GT
%token LEQ
%token GEQ

%token NEQ
%token REGMATCH
%token NREGMATCH

%token FIELDREF
%token ASSIGN

%token IF
%token ELSE
%token PRINT
%token FOR
%token WHILE
%token DO

%token BEGIN
%token END

%token COMMA
%token APPEND
%token SEMICOLON
%token EOF


%start <Ast.code> prog


%nonassoc AND OR
%nonassoc REGMATCH NREGMATCH
%nonassoc EQ NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIV



%nonassoc VAR
%left INCREMENT DECREMENT
%nonassoc LPAREN

%%

prog:
  | e = instructions; EOF { e }
  ;

instructions:
  | p = pattern { [p, []] }
  | LBRACE; list(SEMICOLON); a = bracketed_actions; RBRACE { [Always, a] }
  | p = pattern; is = instructions { 
        match p, is with
        |      _ , (Always,  a)::xs -> (p, a) :: xs
        | Expr e1, (Expr e2, a)::xs -> (Expr(Binop(Concat, e1, e2)), a) :: xs
        |      _ ,               xs -> (p, [ Print [] ]) :: xs 
    }
  | LBRACE; list(SEMICOLON); a = bracketed_actions; RBRACE; is = instructions { (Always, a) :: is }
  ;

  
pattern: /* TODO: allow ORing and ANDing and NOTing patterns */
  | BEGIN     { Begin }
  | END       { End }
  | r = REGEX { Regex r }
  | e = expr1 { Expr e }
  ; 
  
action:
  | s = statement { [s] }
  | LBRACE; RBRACE { [] }
  | LBRACE; list(SEMICOLON); a = bracketed_actions; RBRACE { a }
  ;

bracketed_actions:
  | s = statement { [s] }
  | s = statement; nonempty_list(SEMICOLON); a=bracketed_actions { s::a }
  | s = statement; nonempty_list(SEMICOLON) { [s] }
  ;
  
statement:
  | PRINT {Print []}
  | PRINT; GT; e=base_expr {PrintWrite([], e)}
  | PRINT; APPEND; e=base_expr {PrintAppend([], e)}
  | PRINT; p = print_body {
          match p with
          | es, Normal   -> Print es
          | es, Write e -> PrintWrite(es, e)
          | es, Append e -> PrintAppend(es, e)
          }
  /* | PRINT; LPAREN; es = separated_nonempty_list(COMMA, expr); RPAREN { Print es } */ /* TODO: fix shift/reduce with print ("ala") */
  
  | IF; LPAREN ; e1 = expr; RPAREN ; s = statement { If(e1, [s], [])}
  | IF; LPAREN ; e1 = expr; RPAREN ; LBRACE; list(SEMICOLON); a = bracketed_actions; RBRACE { If(e1, a, []) }
  | IF; LPAREN ; e1 = expr; RPAREN ; LBRACE; list(SEMICOLON); a1 = bracketed_actions; RBRACE; ELSE; a2 = action { If(e1, a1, a2) }
  | FOR; LPAREN; init=expr; SEMICOLON; cond=expr; SEMICOLON; incr=expr; RPAREN; a = action { For(init, cond, incr, a) } /* FIXME: expressions can be omitted, when so they are treated as true */
  | WHILE; LPAREN; e=expr; RPAREN; a = action { While(e, a) }
  | DO; a = action ; WHILE ; LPAREN; e = expr; RPAREN { DoWhile(a, e) }
  | e = expr { ExprStmt e } 
  ;


print_body:
  | e = non_gt_expr {[e], Normal}
  | e1 = non_gt_expr; GT; e2 = expr {([e1], Write e2)}
  | e1 = non_gt_expr; APPEND; e2 = expr {([e1], Append e2)}
  | e = non_gt_expr; COMMA; p = print_body { let (l, r) = p in (e::l, r)}
  ;

variable:
  | x = IDENT { Var x }
  | FIELDREF; n = NUM { FieldRef(Num n) }
  | FIELDREF; LPAREN; e=expr; RPAREN { FieldRef e }

base_expr:
  | LPAREN; e = expr; RPAREN { e }
  | i = NUM { Num i }
  | s = STR { Str s }
  /* distinction as per https://www.gnu.org/software/gawk/manual/html_node/Calling-Built_002din.html
     allow calling built-in functions with or without space, user functions only without */
  | v = variable { VarE v } %prec VAR
  | v = variable; LPAREN; es = separated_list(COMMA, expr); RPAREN { FunctionCall(devar v, es) } /* FIXME: disallow non-built-in functions with space */
  | INCREMENT; x = variable { PreInc x }
  | x = variable; INCREMENT { PostInc x }
  | DECREMENT; x = variable { PreDec x } /* FIXME it should be possible to apply increment/decrement to fields/records  */
  | x = variable; DECREMENT { PostDec x }
  ;
  
expr:
  | v = variable; ASSIGN; e = expr {Assign(v, e)}
  | e1 = expr1; e2 = expr { Binop(Concat, e1, e2) }
  | e = expr1 { e }
  ;
  
expr1:
  | e1 = expr1; GT; e2 = expr1 { Binop(Gt, e1, e2) }
  | e = base_expr { e }
  | e1 = expr1; PLUS; e2 = expr1 { Binop(Add, e1, e2) }
  | e1 = expr1; MINUS; e2 = expr1 { Binop(Sub, e1, e2) }
  | e1 = expr1; DIV; e2 = expr1 { Binop(Div, e1, e2) }
  | e1 = expr1; TIMES; e2 = expr1 { Binop(Mult, e1, e2) }
  | e1 = expr1; EQ; e2 = expr1 { Binop(Eq, e1, e2) }
  | e1 = expr1; LT; e2 = expr1 { Binop(Lt, e1, e2) }
  | e1 = expr1; LEQ; e2 = expr1 { Binop(Le, e1, e2) }
  | e1 = expr1; GEQ; e2 = expr1 { Binop(Ge, e1, e2) }
  | e1 = expr1; NEQ; e2 = expr1 { Binop(Neq, e1, e2) }
  | e1 = expr1; AND; e2 = expr1 { Binop(Land, e1, e2) }
  | e1 = expr1; OR; e2 = expr1 { Binop(Lor, e1, e2) }
  | e1 = expr1; REGMATCH; r = REGEX { Binop(RegMatch, e1, Str r) }
  | e1 = expr1; REGMATCH; e2 = expr1 { Binop(RegMatch, e1, e2) }
  | e1 = expr1; NREGMATCH; r = REGEX { Binop(NRegMatch, e1, Str r) }
  | e1 = expr1; NREGMATCH; e2 = expr1 { Binop(NRegMatch, e1, e2) }
  ;
  
non_gt_expr:
  | v = variable; ASSIGN; e = non_gt_expr {Assign(v, e)}
  | e1 = non_gt_expr1; e2 = non_gt_expr { Binop(Concat, e1, e2) }
  | e = non_gt_expr1 { e }
  ;
  
non_gt_expr1:
  | e = base_expr { e }
  | e1 = non_gt_expr1; PLUS; e2 = non_gt_expr1 { Binop(Add, e1, e2) }
  | e1 = non_gt_expr1; MINUS; e2 = non_gt_expr1 { Binop(Sub, e1, e2) }
  | e1 = non_gt_expr1; DIV; e2 = non_gt_expr1 { Binop(Div, e1, e2) }
  | e1 = non_gt_expr1; TIMES; e2 = non_gt_expr1 { Binop(Mult, e1, e2) }
  | e1 = non_gt_expr1; EQ; e2 = non_gt_expr1 { Binop(Eq, e1, e2) }
  | e1 = non_gt_expr1; LT; e2 = non_gt_expr1 { Binop(Lt, e1, e2) }
  | e1 = non_gt_expr1; LEQ; e2 = non_gt_expr1 { Binop(Le, e1, e2) }
  | e1 = non_gt_expr1; GEQ; e2 = non_gt_expr1 { Binop(Ge, e1, e2) }
  | e1 = non_gt_expr1; NEQ; e2 = non_gt_expr1 { Binop(Neq, e1, e2) }
  | e1 = non_gt_expr1; AND; e2 = non_gt_expr1 { Binop(Land, e1, e2) }
  | e1 = non_gt_expr1; OR; e2 = non_gt_expr1 { Binop(Lor, e1, e2) }
  | e1 = non_gt_expr1; REGMATCH; r = REGEX { Binop(RegMatch, e1, Str r) }
  | e1 = non_gt_expr1; REGMATCH; e2 = non_gt_expr1 { Binop(RegMatch, e1, e2) }
  | e1 = non_gt_expr1; NREGMATCH; r = REGEX { Binop(NRegMatch, e1, Str r) }
  | e1 = non_gt_expr1; NREGMATCH; e2 = non_gt_expr1 { Binop(NRegMatch, e1, e2) }
  ;