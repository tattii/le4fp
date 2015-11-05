(* Exercise 5.3.3 
  与えられたリストのリストに対し,内側のリストの要素を並べたリストを返す関数concat
*)

let rec append l1 l2 =
  match l1 with
    [] -> l2
  | x :: rest -> x :: (append rest l2) ;;

let rec fold_left f e l =
    match l with
      [] -> e
    | x :: rest -> fold_left f (f e x) rest ;;


let concat l =
  fold_left append [] l ;;

concat [[0; 3; 4]; [2]; [5; 0]; []] ;;
