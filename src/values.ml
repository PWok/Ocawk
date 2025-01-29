
type value = 
| VNum of float
| VString of string

type internal_value =
| IVBool of bool
| IVString of string
| IVFileDescriptor of Out_channel.t

module VarMap = Map.Make(String)
type env = value VarMap.t * internal_value VarMap.t

exception InternalValueError of string

module EnvMonad : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val lookup : string -> value t
  val assign : string -> value -> value t
  
  val lookup_internal : string -> internal_value option t
  val assign_internal : string -> internal_value -> internal_value t
  
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
    match VarMap.find_opt ident (fst env) with
    | Some v -> env, v
    | None -> env, VString ""
  
  let assign (ident: string) (v: value) : value t = fun env ->
    let env = VarMap.add ident v (fst env), snd env in
    env, v
  
  let lookup_internal (ident: string) : internal_value option t =  fun env ->
    env, VarMap.find_opt ident (snd env)
    
  let assign_internal (ident: string) (v: internal_value) : internal_value t = fun env ->
    let env = fst env, VarMap.add ident v (snd env) in
    env, v
    
  let view m = m
  
  let (let*) = bind
  let (>>) a b = bind a (fun _ -> b)
end 



let string_of_value v = 
  match v with
  | VNum n    -> 
    if Float.is_integer n 
    then string_of_int @@ int_of_float n
    else string_of_float n
  | VString s -> s
  
let bool_of_value v = 
  match v with
  | VNum n    -> n > 0.
  | VString s -> s = ""
  
let float_of_value v =
  match v with
  | VNum n    -> n
  | VString s ->
    match float_of_string_opt s with (* FIXME: this is not how awk does this see: https://www.gnu.org/software/gawk/manual/html_node/Strings-And-Numbers.html*)
    | None -> 0.
    | Some v -> v

let bool_of_internal_value_option iv = 
  match iv with
  | Some (IVBool b) -> b
  | _ -> raise (InternalValueError "bool of internal failed")
    
let float_of_bool b = if b then 1. else 0.
  
let get_value ident env = ((EnvMonad.lookup ident |> EnvMonad.view) env) |> snd