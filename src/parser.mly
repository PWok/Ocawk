%{
open Ast

type print_type = 
  | Normal
  | Write of expr
  | Append of expr
  
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
%token NOT

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


%left OR
%left AND
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
  | a = bracketed_actions { [Always, a] }
  | p = pattern; is = instructions { 
        match p, is with
        |      _ , (Always,  a)::xs -> (p, a) :: xs
        | Expr e1, (Expr e2, a)::xs -> (Expr(Binop(Concat, e1, e2)), a) :: xs
        |      _ ,               xs -> (p, [ Print [] ]) :: xs 
    }
  | a = bracketed_actions; is = instructions { (Always, a) :: is }
  ;

  
pattern:
  | BEGIN     { Begin }
  | END       { End }
  | r = regex_pattern { RegexC r }
  | e = expr1 { Expr e }
  ; 


base_regex_pattern:
  | r = REGEX { Regex r}
  | LPAREN; r = regex_pattern; RPAREN; { r }

regex_pattern:
  | r1 = regex_pattern; AND; r2 = regex_pattern { RegexAnd(r1, r2) }
  | r1 = regex_pattern; OR; r2 = regex_pattern { RegexOr(r1, r2) }
  | NOT; r = base_regex_pattern { RegexNot r }
  | r = base_regex_pattern { r }
  ;
  
action:
  | s = statement { [s] }
  | LBRACE; RBRACE { [] }
  | a = bracketed_actions { a }
  ;

bracketed_actions:
  | LBRACE; list(SEMICOLON); a = _bracketed; RBRACE { a }
  ;
  
_bracketed:
  | s = statement; nonempty_list(SEMICOLON); a=_bracketed { s::a }
  | s = statement; list(SEMICOLON) { [s] }
  ;
  
statement:
  | PRINT {Print []}
  | PRINT; GT; e=base_expr {PrintWrite([], e)}
  | PRINT; APPEND; e=base_expr {PrintAppend([], e)}
  | PRINT; p = print_body1 {
          match p with
          | es, Normal   -> Print es
          | es, Write e -> PrintWrite(es, e)
          | es, Append e -> PrintAppend(es, e)
          }
  | PRINT; LPAREN; es = print_body2; RPAREN { Print es }
  | PRINT; LPAREN; es = print_body2; RPAREN; GT; e=non_gt_expr { PrintWrite(es, e) }
  | PRINT; LPAREN; es = print_body2; RPAREN; APPEND; e=non_gt_expr { PrintAppend(es, e) }
  | IF; LPAREN ; e1 = expr; RPAREN ; s = statement { If(e1, [s], [])}
  | IF; LPAREN ; e1 = expr; RPAREN ; a = bracketed_actions { If(e1, a, []) }
  | IF; LPAREN ; e1 = expr; RPAREN ; a1 = bracketed_actions; ELSE; a2 = action { If(e1, a1, a2) }
  | FOR; LPAREN; init=option(expr); SEMICOLON; cond=option(expr); SEMICOLON; incr=option(expr); RPAREN; a = action { 
                  let init = Option.value init ~default: (Num 1.) in
                  let cond = Option.value cond ~default: (Num 1.) in
                  let incr = Option.value incr ~default: (Num 1.) in
                  For(init, cond, incr, a)
                  }
  | WHILE; LPAREN; e=option(expr); RPAREN; a = action { 
                  let e = Option.value e ~default: (Num 1.) in
                  While(e, a)
                  }
  | DO; a = action ; WHILE ; LPAREN; e = option(expr); RPAREN {
                  let e = Option.value e ~default: (Num 1.) in
                  DoWhile(a, e)
                  }
  | e = expr { ExprStmt e } 
  ;


print_body1:
  | e = non_gt_expr {[e], Normal}
  | e1 = non_gt_expr; GT; e2 = expr {([e1], Write e2)}
  | e1 = non_gt_expr; APPEND; e2 = expr {([e1], Append e2)}
  | e = non_gt_expr; COMMA; p = print_body1 { let (l, r) = p in (e::l, r)}
  ;
  
print_body2:
  | e1 = expr; COMMA; e2 = expr { [e1;e2] }
  | e = expr; COMMA; p = print_body2 { e::p }

variable:
  | x = IDENT { Var x }
  | FIELDREF; n = NUM { FieldRef(Num n) }
  | FIELDREF; s = STR { FieldRef(Str s) }
  | FIELDREF; v = variable { FieldRef(VarE v)}
  | FIELDREF; LPAREN; e=expr; RPAREN { FieldRef e }

base_expr:
  | NOT; e = base_expr { Not e }
  | LPAREN; e = expr; RPAREN { e }
  | i = NUM { Num i }
  | s = STR { Str s }
  | v = variable { VarE v } %prec VAR
  | v = variable; LPAREN; es = separated_list(COMMA, expr); RPAREN { 
                          match v, es with
                          | Var x, _ -> FunctionCall(x, es)
                          | _, [e]   -> Binop(Concat, VarE v, e)
                          | _, _     -> failwith "You have $( _ ) (_, _ [,_]*) in your code. Thats incorrect syntax. Don't do that."
                          }
  | INCREMENT; x = variable { PreInc x }
  | x = variable; INCREMENT { PostInc x }
  | DECREMENT; x = variable { PreDec x }
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