
type value = 
| VNum of float
| VBool of bool
| VString of string
| VFileDesc of Out_channel.t (* FIXME this is a very stupid solution *)


module VarMap = Map.Make(String)
type env = value VarMap.t

module EnvMonad : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val lookup : string -> value t
  val assign : string -> value -> value t
  
  val view: 'a t -> env -> env * 'a
  
  val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>)  : 'a t -> 'b t -> 'b t
end = struct
  
  
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
    
  let view m = m
  
  let (let*) = bind
  let (>>) a b = bind a (fun _ -> b)
end 


let is_filedesc v =
  match v with
  | VFileDesc _ -> true
  | _ -> false

let filedesc_of_value v = 
  match v with
  | VFileDesc v -> v
  | _ -> failwith "this is absurd (casting not a file descriptor to file descriptor)"

let string_of_value v = 
  match v with
  | VNum n    -> 
    if Float.is_integer n 
    then string_of_int @@ int_of_float n
    else string_of_float n
  | VBool b   -> if b then "1" else "0"
  | VString s -> s
  | VFileDesc _ -> failwith "this is absurd (casting of file descriptor)"
  
let bool_of_value v = 
  match v with
  | VNum n    -> n > 0.
  | VBool b   -> b
  | VString s -> s = ""
  | VFileDesc _ -> failwith "this is absurd (casting of file descriptor)"
  
let float_of_value v =
  match v with
  | VNum n    -> n
  | VBool b   -> if b then 1. else 0.
  | VString s ->
    begin match float_of_string_opt s with (* FIXME: this is not how awk does this see: https://www.gnu.org/software/gawk/manual/html_node/Strings-And-Numbers.html*)
    | None -> 0.
    | Some v -> v
    end
  | VFileDesc _ -> failwith "this is absurd (casting of file descriptor)"

