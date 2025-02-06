open Exceptions

module StrMap = Map.Make(String)

module EnvMonad : sig
  type 'a t
  
  type value = 
  | VNum of float
  | VString of string

  type internal_value =
  | IVBool of bool
  | IVString of string
  | IVFileDescriptor of Out_channel.t
  | IVFunc of (value list -> value t) 

  type env = value StrMap.t * internal_value StrMap.t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val lookup : string -> value t
  val assign : string -> value -> value t
  
  val lookup_internal : string -> internal_value option t
  val assign_internal : string -> internal_value -> unit t
  val remove_internal : string -> unit t
  
  val view: 'a t -> (env -> env * 'a)
  val hide: (env -> env * 'a) -> 'a t
  
  val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>)  : 'a t -> 'b t -> 'b t
end = struct
  type 'a t = env -> env * 'a
  
  and value = 
  | VNum of float
  | VString of string

  and internal_value =
  | IVBool of bool
  | IVString of string
  | IVFileDescriptor of Out_channel.t
  | IVFunc of (value list -> value t) 

  and env = value StrMap.t * internal_value StrMap.t
  
  
  let return (a: 'a) : 'a t = fun env -> env, a

  let bind (a: 'a t) (f: 'a -> 'b t) : 'b t =
    fun env -> 
      let env, va = a env in
      f va env
  
  let lookup (ident: string) : value t =  fun env ->
    match StrMap.find_opt ident (fst env) with
    | Some v -> env, v
    | None -> env, VString ""
  
  let assign (ident: string) (v: value) : value t = fun env ->
    let env = StrMap.add ident v (fst env), snd env in
    env, v
  
  let lookup_internal (ident: string) : internal_value option t =  fun env ->
    env, StrMap.find_opt ident (snd env)
    
  let assign_internal (ident: string) (v: internal_value) : unit t = fun env ->
    let env = fst env, StrMap.add ident v (snd env) in
    env, ()
    
  let remove_internal (ident: string) : unit t = fun env ->
    let env = fst env, StrMap.remove ident (snd env) in
    env, ()
    
  let view m = m
  let hide m = m
  
  let (let*) = bind
  let (>>) a b = bind a (fun _ -> b)
end 

open EnvMonad

let string_of_value v = 
  match v with
  | VNum n    ->
    if Float.is_integer n 
    then string_of_int @@ int_of_float n
    else string_of_float n
  | VString s -> s

  
let bool_of_value v = 
  match v with
  | VNum n    ->
    n <> 0.
  | VString s ->
    match float_of_string_opt s with
    | None -> s <> ""
    | Some v -> v <> 0.0
  
let float_of_value v =
  match v with
  | VNum n    -> n
  | VString s ->
    match float_of_string_opt s with (* FIXME: this is not how awk does this see: https://www.gnu.org/software/gawk/manual/html_node/Strings-And-Numbers.html*)
    | None -> 0.
    | Some v -> v


let string_of_internal_value_option iv =
  match iv with
  | None -> ""
  | Some (IVString s) -> s
  | _ -> raise (InternalValueError "string_of_internal_value_option" )
  
let float_of_bool b = if b then 1. else 0.
  