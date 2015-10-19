(* Excercise 5.3.1
  正の整数nから0までの整数の降順リストを生成する関数 downto0
*)

let downto0 n =
    let rec down(i, l) =
        if i > n then l else down(i + 1, i :: l) in
    down(0, []) ;;

downto0 10 ;;
