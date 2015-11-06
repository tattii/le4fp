(* Exercise 7.6
  多相性と参照 
*)

let x = ref [] ;;
x := [1] ;;
true :: !x ;;
