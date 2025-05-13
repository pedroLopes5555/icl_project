
open Ast
module H = Hashtbl

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string

(* use the following function to signal typing errors, e.g.
      error ~loc "unbound variable %s" id
*)
let error ?(loc=dummy_loc) f =
  Format.kasprintf (fun s -> raise (Error (loc, s)))
    ("@[" ^^ f ^^ "@]")

type fn_env = (string, fn) H.t

let fn_env : fn_env = H.create 16

let dummy_var = { v_name = "dummy"; v_ofs = -1 }

let () =
  H.add fn_env "len" { fn_name = "len"; fn_params = [dummy_var] }

type var_env = (string, var) H.t

let rec expr (ctx: var_env) (e: Ast.expr) =
  match e with
  | Ecst c -> TEcst c
  | Eident x -> begin
      try
        let v = H.find ctx x.id in
        TEvar v
      with Not_found ->
        error ~loc:x.loc "Unbound variable %s" x.id
    end
  | Ebinop (op, e1, e2) ->
      TEbinop (op, expr ctx e1, expr ctx e2)
  | Eunop (op, e) ->
      TEunop (op, expr ctx e)
  | Ecall (f, args) -> begin
      try
        let fn = H.find fn_env f.id in
        TEcall (fn, List.map (expr ctx) args)
      with Not_found ->
        error ~loc:f.loc "Function %s " f.id " not found"
    end
  | Elist e ->
      TElist (List.map (expr ctx) e)
  | Eget (e1, e2) ->
      TEget (expr ctx e1, expr ctx e2)

let rec stmt (ctx: var_env) (s: Ast.stmt) : Ast.tstmt =
  match s with
  | Sif (e, s1, s2) ->
      TSif (expr ctx e, stmt ctx s1, stmt ctx s2)
  | Sreturn e ->
      TSreturn (expr ctx e)
  | Sassign (x, e) ->
      let v = H.find ctx x.id (* this should never fail *) in
      TSassign (v, expr ctx e)
  | Sprint e ->
      TSprint (expr ctx e)
  | Sblock sl ->
      TSblock (List.map (stmt ctx) sl)
  | Sfor (x, e, s) -> assert false (* TODO *)
  | Seval e ->
      TSeval (expr ctx e)
  | Sset (e1, e2, e3) ->
      TSset (expr ctx e1, expr ctx e2, expr ctx e3)

let alloc_var ctx x =
  if not (H.mem ctx x.id) then
    H.add ctx x.id { v_name = x.id; v_ofs = -1 }

let rec alloc_vars ctx (s: Ast.stmt) =
  match s with
  | Sif (_, s1, s2) ->
      alloc_vars ctx s1; alloc_vars ctx s2
  | Sassign (x, _) ->
      alloc_var ctx x
  | Sblock sl ->
      List.iter (alloc_vars ctx) sl
  | Sfor (x, _, s) ->
      alloc_var ctx x; alloc_vars ctx s
  | Sprint _ | Sreturn _ | Seval _ | Sset (_, _, _) -> ()

let def (f, args, body) =
  let mk_var x = { v_name = x.id; v_ofs = -1 } in
  let targs = List.map mk_var args in
  let fn = { fn_name = f.id; fn_params = targs } in
  H.add fn_env f.id fn;
  let ctx = H.create 32 in (* empty context *)
  List.iter (fun v -> H.add ctx v.v_name v) targs;
  alloc_vars ctx body;
  let tbody = stmt ctx body in
  (fn, tbody)

let file ?debug:(b=false) (p: Ast.file) : Ast.tfile =
  debug := b;
  let defs, s = p in
  (* typing all the declared functions *)
  let tdefs = List.map def defs in
  let ctx = H.create 32 in (* empty context *)
  alloc_vars ctx s;
  let ts = stmt ctx s in
  let fn_main = { fn_name = "main"; fn_params = [] } in
  let tmain = (fn_main, ts) in
  tmain :: tdefs