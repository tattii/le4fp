open Syntax 

type exval = 
    IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t ref
  | ListV of exp list
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV _ -> "<fun>"
  | ListV l -> "list"

let pp_val v = print_string (string_of_exval v)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Minus, IntV i1, IntV i2 -> IntV (i1 - i2)
  | Minus, _, _ -> err ("Both arguments must be integer: -")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | AND, BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | AND, _, _ -> err ("Both arguments must be bool: &&")
  | OR, BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | OR, _, _ -> err ("Both arguments must be bool: ||")

let rec eval_exp env = function
    Var x -> 
      (try Environment.lookup x env with 
        Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) -> 
      let arg1 = eval_exp env exp1 in
      let arg2 = eval_exp env exp2 in
      apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
      let test = eval_exp env exp1 in
        (match test with
            BoolV true -> eval_exp env exp2 
          | BoolV false -> eval_exp env exp3
          | _ -> err ("Test expression must be boolean: if"))
  | LetExp (id, exp1, exp2) ->
      let value = eval_exp env exp1 in
        eval_exp (Environment.extend id value env) exp2
  | FunExp (id, exp) -> ProcV (id, exp, ref env)
  | AppExp (exp1, exp2) ->
      let funval = eval_exp env exp1 in
      let arg = eval_exp env exp2 in
       (match funval with
            ProcV (id, body, env') ->
              let newenv = Environment.extend id arg !env' in
                eval_exp newenv body
          | _ -> err ("Non-function value is applied"))
  | LetRecExp (id, para, exp1, exp2) ->
      let dummyenv = ref Environment.empty in
      let newenv = Environment.extend id (ProcV (para, exp1, dummyenv)) env in
        dummyenv := newenv;
        eval_exp newenv exp2
  | MatchExp (target_exp, exp1, id1, id2, exp2) ->
      let target = eval_exp env target_exp in
        (match target with
            ListV list ->
              (match list with
                  [] -> eval_exp env exp1
                | x :: rest ->
                    let env2 = Environment.extend id1 (eval_exp env x) env in
                    let env3 = Environment.extend id2 (ListV rest) env2 in
                      eval_exp env3 exp2)
          | _ -> err ("it only matches with list"))

let eval_decl env = function
    Decl (id, e) ->
      let v = eval_exp env e in (id, Environment.extend id v env, v)
  | RecDecl (id, para, e) ->
      let dummyenv = ref Environment.empty in
      let v = ProcV (para, e, dummyenv) in
      let newenv = Environment.extend id v env in
        dummyenv := newenv;
        (id, newenv, v)

let rec eval_decls env = function
    [] -> []
  | decl ::rest ->
      let (id, newenv, v) = eval_decl env decl in
        (id, newenv, v) :: eval_decls newenv rest

let eval_decl_list env = function
    Exp e -> let v = eval_exp env e in [("-", env, v)]
  | DeclList decls -> eval_decls env decls
