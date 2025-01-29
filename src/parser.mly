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
%token TIMES
%token DIV
%token PLUS
%token MINUS
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token AND
%token OR
%token EQ
%token LT
%token GT
%token LEQ
%token GEQ
%token NEQ
%token IF
%token ELSE
%token COMMA
%token PRINT
%token BEGIN
%token END
%token SEMICOLON
%token NEWLINE
%token EOF
%token REGMATCH
%token NREGMATCH
%token ASSIGN
%token APPEND

%start <Ast.code> prog


%right ASSIGN

%nonassoc AND OR
%nonassoc REGMATCH NREGMATCH
%nonassoc EQ NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIV

%left CONCAT

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
  | PRINT; GT; e=str_expr {PrintWrite([], e)}
  | PRINT; APPEND; e=str_expr {PrintAppend([], e)}
  | PRINT; p = print_body {
          match p with
          | es, Normal   -> Print es
          | es, Write e -> PrintWrite(es, e)
          | es, Append e -> PrintAppend(es, e)
          }
/*   | PRINT; LPAREN; es = separated_list(COMMA, expr); RPAREN { Print es } */ /* TODO fix shift reduce and add this */
  | IF; LPAREN ; e1 = expr; RPAREN ; s = statement { If(e1, [s], [])}
  | IF; LPAREN ; e1 = expr; RPAREN ; LBRACE; list(seperator); e2 = bracketed_actions; RBRACE; { If(e1, e2, []) }
  | IF; LPAREN ; e1 = expr; RPAREN ; LBRACE; list(seperator); e2 = bracketed_actions; RBRACE; ELSE; e3 = action { If(e1, e2, e3) }
  | e = expr { ExprStmt e } 
  ;


print_body:
  | e = non_gt_expr {[e], Normal}
  | e1=non_gt_expr; GT; e2=str_expr {([e1], Write e2)}
  | e1=non_gt_expr; APPEND; e2=str_expr {([e1], Append e2)}
  | e = non_gt_expr; COMMA; p=print_body { let (l, r) = p in (e::l, r)}
  ;


str_expr:
  | LPAREN; e = expr; RPAREN { e }
  | i = NUM { Num i }
  | s = STR { Str s }
  | x = IDENT { Var x }
 
  ;
  
expr:
  | e1 = expr; GT; e2 = expr { Binop(Gt, e1, e2) }
  | e = str_expr { e }
  | e1 = str_expr; e2 = expr { Binop(Concat, e1, e2) } %prec CONCAT
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
  | id = IDENT; ASSIGN; e = expr {Assign(id, e)}
  ;
  
non_gt_expr:
  | e = str_expr { e }
  | e1 = str_expr; e2 = non_gt_expr { Binop(Concat, e1, e2) } %prec CONCAT
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
  | id = IDENT; ASSIGN; e = non_gt_expr {Assign(id, e)}
  ;
