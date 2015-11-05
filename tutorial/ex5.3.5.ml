(* Excercise 5.3.5

  リストと,リストの要素上の述語(bool型を返す関数) p をとって,
  p を満たす全ての要素のリストを返すfilter.

  # let positive x = (x > 0) ;;
  # filter positive [-9; 0; 2; 5; -3] ;;

*)

let rec filter p = function
    [] -> []
  | x :: rest -> if (p x) then x :: (filter p rest) else (filter p rest) ;;


let positive x = (x > 0) ;;
filter positive [-9; 0; 2; 5; -3] ;;
