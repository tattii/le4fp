(* Exercise 7.6
  多相性と参照 
*)

let x = ref [] ;;
x := [1] ;;
(* true :: !x ;; *)

(*
   これを実行すると,
   Error: This expression has type int list
          but an expression was expected of type bool list
          Type int is not compatible with type bool

  というエラーが発生する.
  コンパイラがconsする際の2つの値の型を確認しているため,
  true を [1] にconsしてしまうような事態は発生しない.
*)
