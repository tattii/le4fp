(* Exercise 7.2
  与えられた参照の指す先の整数を 1 増やす関数 incr を定義せよ.
*)

let incr x =
    x := !x + 1 ;;

let x = ref 3 ;;
incr x ;;
!x ;;

