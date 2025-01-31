%{
open Ast

type print_type = 
  | Normal
  | Write of expr
  | Append of expr
  
let devar v = 
  match v with
  | Var x -> x
  | _ -> failwith "this shouldn't ever happen... (parser devar)"

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
%token NEWLINE
%token EOF


%start <Ast.code> prog


%right ASSIGN

%right CONCAT

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
  
seperator:
  | NEWLINE   {}
  | SEMICOLON {}

instructions:
  | i = instruction { [i] }
  | i = instruction; list(NEWLINE); is = instructions { 
        match i, is with
        | (p, []), ([], a)::xs -> (p, a) :: xs
        | (p, []), xs -> (p, [ Print [] ]) :: xs
        | _ -> i::is     
    }
  ;

instruction: 
  | LBRACE; list(seperator); a = bracketed_actions; RBRACE { [], a }
  | p = pattern { [p], []}
  ;
  
pattern: /* TODO: allow ORing and ANDing and NOTing patterns */
  | BEGIN     { Begin }
  | END       { End }
  | r = REGEX { Regex r }
  /* | e = expr { Expr e } */ /* TODO -- conflict with concat */
  | LPAREN; e = expr; RPAREN  { Expr e }
  ; 

  
action:
  | s = statement { [s] }
  | LBRACE; RBRACE { [] }
  | LBRACE; list(seperator); a = bracketed_actions; RBRACE { a }
  ;

bracketed_actions:
  | s = statement { [s] }
  | s = statement; nonempty_list(seperator); a=bracketed_actions { s::a }
  | s = statement; nonempty_list(seperator) { [s] }
  ;
  
statement:
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
  | IF; LPAREN ; e1 = expr; RPAREN ; LBRACE; list(seperator); a = bracketed_actions; RBRACE { If(e1, a, []) }
  | IF; LPAREN ; e1 = expr; RPAREN ; LBRACE; list(seperator); a1 = bracketed_actions; RBRACE; ELSE; a2 = action { If(e1, a1, a2) }
  | FOR; LPAREN; init=expr; SEMICOLON; cond=expr; SEMICOLON; incr=expr; RPAREN; a = action { For(init, cond, incr, a) } /* FIXME: expressions can be omitted, when so they are treated as true */
  | WHILE; LPAREN; e=expr; RPAREN; a = action { While(e, a) }
  | DO; a = action ; WHILE ; LPAREN; e = expr; RPAREN { DoWhile(a, e) }
  | e = expr { ExprStmt e } 
  ;


print_body: /*  */
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
  | INCREMENT; x = variable { PreInc (devar x) }
  | x = variable; INCREMENT { PostInc (devar x) }
  | DECREMENT; x = variable { PreDec (devar x) }
  | x = variable; DECREMENT { PostDec (devar x) }
  ;
  
expr:
  | e1 = expr; GT; e2 = expr { Binop(Gt, e1, e2) }
  | e = base_expr { e }
  | e1 = base_expr; e2 = expr { Binop(Concat, e1, e2) } %prec CONCAT /* FIXME: is this correct? test '{print 1 + 2 "3"}'. Change non_gt_expr too. */
  | e1 = expr; PLUS; e2 = expr { Binop(Add, e1, e2) }
  | e1 = expr; MINUS; e2 = expr { Binop(Sub, e1, e2) }
  | e1 = expr; DIV; e2 = expr { Binop(Div, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binop(Mult, e1, e2) }
  | e1 = expr; EQ; e2 = expr { Binop(Eq, e1, e2) }
  | e1 = expr; LT; e2 = expr { Binop(Lt, e1, e2) }
  | e1 = expr; LEQ; e2 = expr { Binop(Le, e1, e2) }
  | e1 = expr; GEQ; e2 = expr { Binop(Ge, e1, e2) }
  | e1 = expr; NEQ; e2 = expr { Binop(Neq, e1, e2) }
  | e1 = expr; AND; e2 = expr { Binop(Land, e1, e2) }
  | e1 = expr; OR; e2 = expr { Binop(Lor, e1, e2) }
  | e1 = expr; REGMATCH; r = REGEX { Binop(RegMatch, e1, Str r) }
  | e1 = expr; REGMATCH; e2 = expr { Binop(RegMatch, e1, e2) }
  | e1 = expr; NREGMATCH; r = REGEX { Binop(NRegMatch, e1, Str r) }
  | e1 = expr; NREGMATCH; e2 = expr { Binop(NRegMatch, e1, e2) }
  | v = variable; ASSIGN; e = expr {Assign(v, e)}
  ;
  
non_gt_expr:
  | e = base_expr { e }
  | e1 = base_expr; e2 = non_gt_expr { Binop(Concat, e1, e2) } %prec CONCAT 
  | e1 = non_gt_expr; PLUS; e2 = non_gt_expr { Binop(Add, e1, e2) }
  | e1 = non_gt_expr; MINUS; e2 = non_gt_expr { Binop(Sub, e1, e2) }
  | e1 = non_gt_expr; DIV; e2 = non_gt_expr { Binop(Div, e1, e2) }
  | e1 = non_gt_expr; TIMES; e2 = non_gt_expr { Binop(Mult, e1, e2) }
  | e1 = non_gt_expr; EQ; e2 = non_gt_expr { Binop(Eq, e1, e2) }
  | e1 = non_gt_expr; LT; e2 = non_gt_expr { Binop(Lt, e1, e2) }
  | e1 = non_gt_expr; LEQ; e2 = non_gt_expr { Binop(Le, e1, e2) }
  | e1 = non_gt_expr; GEQ; e2 = non_gt_expr { Binop(Ge, e1, e2) }
  | e1 = non_gt_expr; NEQ; e2 = non_gt_expr { Binop(Neq, e1, e2) }
  | e1 = non_gt_expr; AND; e2 = non_gt_expr { Binop(Land, e1, e2) }
  | e1 = non_gt_expr; OR; e2 = non_gt_expr { Binop(Lor, e1, e2) }
  | e1 = non_gt_expr; REGMATCH; r = REGEX { Binop(RegMatch, e1, Str r) }
  | e1 = non_gt_expr; REGMATCH; e2 = non_gt_expr { Binop(RegMatch, e1, e2) }
  | e1 = non_gt_expr; NREGMATCH; r = REGEX { Binop(NRegMatch, e1, Str r) }
  | e1 = non_gt_expr; NREGMATCH; e2 = non_gt_expr { Binop(NRegMatch, e1, e2) }
  | v = variable; ASSIGN; e = non_gt_expr {Assign(v, e)}
  ;


