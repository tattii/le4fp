open Syntax
open Eval

let print_val (id, v) =
  Printf.printf "val %s = " id;
  pp_val v;
  print_newline()


let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  try 
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let decls = eval_decl_list env decl in
    let rec print_decls env = function
        [] -> read_eval_print env
      | (id, newenv, v) :: rest ->
          print_val (id, v);
          print_decls newenv rest
    in print_decls env decls
  with
      Eval.Error e ->
        Printf.printf "[error] Eval error: %s" e;
        print_newline();
        read_eval_print env
    | Failure e ->
        Printf.printf "[error] %s" e;
        print_newline();
        read_eval_print env
    | Parsing.Parse_error ->
        Printf.printf "[error] Parse error";
        print_newline();
        read_eval_print env

let initial_env = 
  Environment.extend "i" (IntV 1)
    (Environment.extend "ii" (IntV 2) 
      (Environment.extend "iii" (IntV 3) 
        (Environment.extend "iv" (IntV 4) 
          (Environment.extend "v" (IntV 5) 
            (Environment.extend "x" (IntV 10) Environment.empty)))))

let _ = read_eval_print initial_env
