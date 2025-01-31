{
open Parser

let string_buff = Buffer.create 256

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

}

let backslash_escapes =
    ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

let white = [' ' '\t']+
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let number = '-'? digit+ ('.' digit+)?
let ident = (letter) (letter | digit)*

rule read =
  parse
  | "\n" { NEWLINE }
  | ";"  { SEMICOLON }
  | white { read lexbuf }
  | ">>" { APPEND }
  | "&&" { AND }
  | "||" { OR }
  | "=" { ASSIGN } 
  | "==" { EQ }
  | "!=" { NEQ }
  (* | "!"  { NOT } *) (* TODO *)
  | "$" { FIELDREF }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | "<" { LT }
  | ">" { GT }
  | "++" { INCREMENT }
  | "--" { DECREMENT }
  | "*" { TIMES }
  | "+" { PLUS }
  | "-" { MINUS }
  | "/" { DIV }
  | "~" { REGMATCH }
  | "!~" { NREGMATCH }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "if" { IF }
  | "else" { ELSE }
  | "for" { FOR }
  | "while" { WHILE }
  | "do" { DO }
  | "," { COMMA }
  | "BEGIN" { BEGIN }
  | "END" { END }
  | "print" { PRINT }
  | "/" (_+ as r) "/" { REGEX r }
  | '"'
    { Buffer.clear string_buff;
      string lexbuf;
      STR(Buffer.contents string_buff) }
  | number { NUM (float_of_string (Lexing.lexeme lexbuf)) }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | eof { EOF }

and string = (* This *definitely* isnt just copy-pasted from stack overflow. definitely. *)
  parse 
  | '"'
      { () }
  | '\\' (backslash_escapes as c)
      { Buffer.add_char string_buff (char_for_backslash c);
        string lexbuf }
  | _ as c
      { Buffer.add_char string_buff c;
        string lexbuf }