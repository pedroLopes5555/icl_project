module H = Hashtbl
val debug : bool ref
val dummy_loc : Lexing.position * Lexing.position
exception Error of Ast.location * string
val error :
  ?loc:Ast.location ->
  ('a, Format.formatter, unit, unit, unit, 'b) format6 -> 'a
type fn_env = (string, Ast.fn) H.t
val fn_env : fn_env
val dummy_var : Ast.var
type var_env = (string, Ast.var) H.t
val expr : var_env -> Ast.expr -> Ast.texpr
val stmt : var_env -> Ast.stmt -> Ast.tstmt
val alloc_var : (string, Ast.var) H.t -> Ast.ident -> unit
val alloc_vars : (string, Ast.var) H.t -> Ast.stmt -> unit
val def : Ast.ident * Ast.ident list * Ast.stmt -> Ast.fn * Ast.tstmt
val file : ?debug:bool -> Ast.file -> Ast.tfile
