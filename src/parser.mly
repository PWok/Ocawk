%{
open Ast
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
%token STMT_SEP
%token EOF
%token REGMATCH
%token NREGMATCH
%token ASSIGN


%start <Ast.code> prog


%right ASSIGN

%nonassoc AND OR
%nonassoc REGMATCH NREGMATCH
%nonassoc EQ NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIV

%%

prog:
  | e = instructions; EOF { e }
  ;

instructions:
  | i = instruction { [i] }
  | i = instruction; is = instructions { 
        match i, is with
        | (p, []), ([], a)::xs -> (p, a) :: xs
        | (p, []), xs -> (p, [ Print [] ]) :: xs
        | _ -> i::is     
    }
  ;

instruction: 
  | LBRACE; list(STMT_SEP); a = bracketed_actions; RBRACE { [], a }
  | p = pattern { [p], []}
  ;
  
pattern: /* TODO: allow ORing and ANDing and NOTing patterns */
  | BEGIN     { Begin }
  | END       { End }
  | r = REGEX { Regex r }
  /* | e = expr { Expr e } */
  ; 


  
action:
  | s = statement { [s] }
  | LBRACE; RBRACE { [] }
  | LBRACE; list(STMT_SEP); a = bracketed_actions; RBRACE { a }
  ;

bracketed_actions:
  | s = statement { [s] }
  | s = statement; nonempty_list(STMT_SEP); a=bracketed_actions { s::a }
  | s = statement; nonempty_list(STMT_SEP) { [s] }
  ;
  
statement:
  | PRINT; es = separated_list(COMMA, expr) { Print es }
/*   | PRINT; LPAREN; es = separated_list(COMMA, expr); RPAREN { Print es } */ /* TODO fix shift reduce and add this */
  | IF; LPAREN ; e1 = expr; RPAREN ; s = statement { If(e1, [s], [])}
  | IF; LPAREN ; e1 = expr; RPAREN ; LBRACE; list(STMT_SEP); e2 = bracketed_actions; RBRACE; { If(e1, e2, []) }
  | IF; LPAREN ; e1 = expr; RPAREN ; LBRACE; list(STMT_SEP); e2 = bracketed_actions; RBRACE; ELSE; e3 = action { If(e1, e2, e3) }
  | e = expr { ExprStmt e } 
  ;

expr:
  | LPAREN; e = expr; RPAREN { e }
  | i = NUM { Num i }
  | s = STR { Str s }
  | x = IDENT { Var x }
  | e1 = expr; PLUS; e2 = expr { Binop(Add, e1, e2) }
  | e1 = expr; MINUS; e2 = expr { Binop(Sub, e1, e2) }
  | e1 = expr; DIV; e2 = expr { Binop(Div, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binop(Mult, e1, e2) }
  | e1 = expr; EQ; e2 = expr { Binop(Eq, e1, e2) }
  | e1 = expr; LT; e2 = expr { Binop(Lt, e1, e2) }
  | e1 = expr; GT; e2 = expr { Binop(Gt, e1, e2) }
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
