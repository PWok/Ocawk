
exception FunctionCallError of string

exception InternalValueError of string

exception Parse_error of Lexing.position * string