open Values
open Exceptions

open Values.EnvMonad

let _atan2 values = 
  match values with
  | [y; x] -> 
    return @@ VNum( atan2 (float_of_value y) (float_of_value x))
  | _ -> raise @@ FunctionCallError ("atan2 function expects 2 argument, got " ^ (string_of_int (List.length values)))
;;
let _cos values = 
  match values with
  | [x] -> 
    return @@ VNum( cos @@ float_of_value x)
  | _ -> raise @@ FunctionCallError ("cos function expects 1 argument, got " ^ (string_of_int (List.length values)))
;;
let _exp values = 
  match values with
  | [x] -> 
    return @@ VNum( cos @@ float_of_value x)
  | _ -> raise @@ FunctionCallError ("exp function expects 1 argument, got " ^ (string_of_int (List.length values)))
;;
let _int values = 
  match values with
  | [x] -> 
    return @@ VNum( Float.trunc @@ float_of_value x)
  | _ -> raise @@ FunctionCallError ("int function expects 1 argument, got " ^ (string_of_int (List.length values)))
;;
let _log values = 
  match values with
  | [x] -> 
    return @@ VNum( log @@ float_of_value x)
  | _ -> raise @@ FunctionCallError ("log function expects 1 argument, got " ^ (string_of_int (List.length values)))
;;
(* FIXME: add randomness -- functions rand and srand. Store seed in internal_values *)
let _sin values = 
  match values with
  | [x] -> 
    return @@ VNum( sin @@ float_of_value x)
  | _ -> raise @@ FunctionCallError ("sin function expects 1 argument, got " ^ (string_of_int (List.length values)))
;;
let _sqrt values = 
  match values with
  | [x] -> 
    return @@ VNum( sqrt @@ float_of_value x)
  | _ -> raise @@ FunctionCallError ("sqrt function expects 1 argument, got " ^ (string_of_int (List.length values)))
;;
let _length values = 
  match values with
  | [x] -> 
    return @@ VNum( float_of_int @@ String.length @@ string_of_value x)
  | _ -> raise @@ FunctionCallError ("length function expects 1 argument, got " ^ (string_of_int (List.length values)))
;;

let _close values =
  let helper x = 
    let file_name = string_of_value x in
    let* file_desc = lookup_internal ("file_" ^ file_name) in
    match file_desc with
    | Some (IVFileDescriptor fd) -> Out_channel.close fd; return @@ VString ""
    | Some _ -> raise (InternalValueError ("close called with" ^ file_name ^ " which does not point to file"))
    | _ -> return @@ VString "" (* Tried to close somethong thats not open -- just ignore it *)
  in
  match values with
  | [x] -> helper x
  | [x; _] ->
    Printf.eprintf "The second argument of close function is currently ignored, as pipes are not implemented.";
    helper x
  | _ -> raise @@ FunctionCallError ("close function expects 1 or 2 arguments, got " ^ (string_of_int (List.length values)))
let value_env = 
  StrMap.empty                          |> 
  StrMap.add "FS"   (VString  " ")      |>
  StrMap.add "RS"   (VString "\n")      |> 
  StrMap.add "OFS"  (VString  " ")      |>
  StrMap.add "ORS"  (VString "\n")      |> 
  StrMap.add "NR"   (VNum 0.)           |>
  StrMap.add "FNR"  (VNum 0.)           |>
  StrMap.add "NF"   (VNum 0.)           |>
  StrMap.add "CONVFMT" (VString "%.6g") |> (* FIXME: Currently unused. add this to converting nums to str *)
  StrMap.add "OFMT" (VString "%.6g")    |>
  StrMap.add "FILENAME" (VString "")
  (* TODO: these are not all: see https://www.gnu.org/software/gawk/manual/html_node/Built_002din-Variables.html *)

  
let internal_env = 
  StrMap.empty                             |> 
  StrMap.add "func_atan2" (IVFunc _atan2)  |> 
  StrMap.add "func_cos" (IVFunc _cos)      |> 
  StrMap.add "func_exp" (IVFunc _exp)      |> 
  StrMap.add "func_int" (IVFunc _int)      |> 
  StrMap.add "func_log" (IVFunc _log)      |> 
  StrMap.add "func_sin" (IVFunc _sin)      |> 
  StrMap.add "func_sqrt" (IVFunc _sqrt)    |> 
  StrMap.add "func_length" (IVFunc _length)|>
  StrMap.add "func_close" (IVFunc _close)
  
(* FIXME: add more functions: https://www.gnu.org/software/gawk/manual/html_node/Built_002din.html *)