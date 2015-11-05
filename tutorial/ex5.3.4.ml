(* Exercise 5.3.4

  ２つのリスト [a1; ...; an] と [b1; ...; bn] を引数として,
  [(a1, b1); ...; (an, bn)] を返す関数zip.

  (与えられたリストの長さが異なる場合は長いリストの余った部分を捨ててよい.)
*)

let rec zip a b =
  match a with
    [] -> []
  | ax :: arest -> match b with
      [] -> []
    | bx :: brest -> (ax, bx) :: zip arest brest ;;

zip [1; 2; 3] [-1; -2] ;;
