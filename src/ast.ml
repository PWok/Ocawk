(* abstract syntax tree *)

(* TODO: Add more operators *)
(* https://www.gnu.org/software/gawk/manual/html_node/Comparison-Operators.html *)
(* https://www.gnu.org/software/gawk/manual/html_node/Arithmetic-Ops.html *)
type bop = Mult | Div | Add | Sub | Eq | Lt | Le | Gt | Ge | Neq
          | Land | Lor | RegMatch | NRegMatch | Concat
          
(* FIXME: $0 to nie zmienna. $ to operator `field reference`. Meaning that $(i++) is valid... *)
          
type expr =
  | Num   of float
  | Str   of string
  | Binop of bop * expr * expr
  | Var   of string
  | Assign of string * expr
  | PreInc of string     (* ++x *)
  | PostInc of string    (* x++ *)
  | PreDec of string     (* --x *)
  | PostDec of string    (* x-- *)
  
type stmt = (* TODO: add more statemnts eg. printf, switch, do-while etc. https://www.gnu.org/software/gawk/manual/html_node/Statements.html *)
  | Print  of expr list
  | PrintWrite of expr list * expr (* TODO: add pipes *)
  | PrintAppend of expr list * expr
  | If     of expr * stmt list * stmt list
  | For of expr * expr * expr * stmt list
  | While of expr * stmt list
  | DoWhile of stmt list * expr
  | ExprStmt of expr (* Because for some reason assignment is an expression in awk 
                        and has the value of RHS, so we have to do this *)
  (* TODO: add close function *)
        
type condition =
  | Regex of string
  | Expr  of expr
  | Begin
  | End


type instruction = condition list * stmt list

type code = instruction list