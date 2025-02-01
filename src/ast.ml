(* abstract syntax tree *)

(* TODO: Add more operators *)
(* https://www.gnu.org/software/gawk/manual/html_node/Comparison-Operators.html *)
(* https://www.gnu.org/software/gawk/manual/html_node/Arithmetic-Ops.html *)
type bop = Mult | Div | Add | Sub | Eq | Lt | Le | Gt | Ge | Neq
          | Land | Lor | RegMatch | NRegMatch | Concat


type expr =
  | VarE of variable
  | Num   of float
  | Str   of string
  | Not of expr
  | Binop of bop * expr * expr
  | Assign of variable * expr
  | PreInc of variable     (* ++x *)
  | PostInc of variable    (* x++ *)
  | PreDec of variable     (* --x *)
  | PostDec of variable    (* x-- *)
  | FunctionCall of string * expr list
and variable =
  | Var of string
  | FieldRef of expr
  
type stmt = (* TODO: add more statemnts eg. printf, switch etc. https://www.gnu.org/software/gawk/manual/html_node/Statements.html *)
  | Print  of expr list
  | PrintWrite of expr list * expr (* TODO: add pipes *)
  | PrintAppend of expr list * expr
  | If     of expr * stmt list * stmt list
  | For of expr * expr * expr * stmt list
  | While of expr * stmt list
  | DoWhile of stmt list * expr
  | ExprStmt of expr (* Because for some reason assignment is an expression in awk 
                        and has the value of RHS, so we have to do this *)
        
type regex_condition =
  | Regex of string
  | RegexAnd of regex_condition * regex_condition
  | RegexOr of regex_condition * regex_condition
  | RegexNot of regex_condition
  
type condition =
  | Always
  | RegexC of regex_condition
  | Expr  of expr
  | Begin
  | End


type instruction = condition * stmt list

type code = instruction list