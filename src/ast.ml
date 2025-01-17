(* abstract syntax tree *)

(* TODO: Add more operators *)
(* https://www.gnu.org/software/gawk/manual/html_node/Comparison-Operators.html *)
(* https://www.gnu.org/software/gawk/manual/html_node/Arithmetic-Ops.html *)
type bop = Mult | Div | Add | Sub | Eq | Lt | Le | Gt | Ge | Neq
          | Land | Lor | RegMatch | NRegMatch

type ident = string
          
type expr = 
  | Num   of float
  | Str   of string
  | Binop of bop * expr * expr
  | Var   of ident
  | Assign of ident * expr

type stmt = (* TODO: add more statemnts eg. printf, loops etc. *)
  | Print  of expr list
  | If     of expr * stmt list * stmt list
  | ExprStmt of expr (* Because for some reason assignment is an expression in awk 
                        and has the value of RHS, so we have to do this *)
  
type condition =
  | Regex of string
  | Expr  of expr
  | Begin
  | End


type instruction = condition list * stmt list

type code = instruction list