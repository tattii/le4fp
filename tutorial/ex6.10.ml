(* Exercise 6.10
  以下で定義される ('a, 'b) sum 型は, 
  "α 型の値もしくは β 型の値"という和集合的なデータの構成を示す型である.
 
  type ('a, 'b) sum = Left of 'a | Right of 'b ;;

  これを踏まえて、次の型をもつ関数を定義せよ.
    1. 'a * ('b, 'c) sum -> ('a * 'b, 'a * 'c) sum 
    2. ('a, 'b) sum * ('c, 'd) sum ->
         (('a * 'c, 'd * 'd) sum, ('a * 'd, 'b * 'c) sum) sum
*)

type ('a, 'b) sum = Left of 'a | Right of 'b ;;

(* 1. 'a * ('b, 'c) sum -> ('a * 'b, 'a * 'c) sum *)
let a1 a = function
    Left b -> Left (a, b)
  | Right c -> Right (a, c) ;;

(* 2. ('a, 'b) sum * ('c, 'd) sum ->
        (('a * 'c, 'd * 'd) sum, ('a * 'd, 'b * 'c) sum) sum *)
let a2 = function
      (Left a, Left c) -> Left (Left (a, c))
    | (Left a, Right d) -> Right (Left (a, d))
    | (Right b, Left c) -> Right (Right (b, c))
    | (Right b, Right d) -> Left (Right (d, d)) ;;
