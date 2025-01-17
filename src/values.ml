
type value = 
| VNum of float
| VBool of bool
| VString of string
      
module EnvMonad : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val lookup : string -> value t
  val assign : string -> value -> value t
end = struct
  
  module VarMap = Map.Make(String)
  type env = value VarMap.t
  
  type 'a t = env -> env * 'a
  let return (a: 'a) : 'a t = fun env -> env, a

  let bind (a: 'a t) (f: 'a -> 'b t) : 'b t =
    fun env -> 
      let env, va = a env in
      f va env
  
  let lookup (ident: string) : value t =  fun env ->
    match VarMap.find_opt ident env with
    | Some v -> env, v
    | None -> env, VString ""
  
  let assign (ident: string) (v: value) : value t = fun env ->
    let env = VarMap.add ident v env in
    env, v
end 




let string_of_value v = 
  match v with
  | VNum n    -> string_of_float n
  | VBool b   -> string_of_bool b
  | VString s -> s
  
let bool_of_value v = 
  match v with
  | VNum n    -> n > 0.
  | VBool b   -> b
  | VString s -> s = ""
  
let float_of_value v =
  match v with
  | VNum n    -> n
  | VBool b   -> if b then 1. else 0.
  | VString s -> if s = "" then 0. else float_of_string s (* FIXME: this is not how awk does this see: https://www.gnu.org/software/gawk/manual/html_node/Strings-And-Numbers.html*)
