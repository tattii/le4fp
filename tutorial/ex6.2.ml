(* Exercise 6.2

  nat 型の値をそれが表現する int に変換する関数 int_of_nat
  nat 上の掛け算を行う関数 mul
  nat 上の引き算を行う関数 monus (ただし 0 - n = 0)
  を定義せよ.

  (mul, monus は *, - などの助けを借りず, nat 型の値から"直接"計算するようにせよ.)
 
*)

type nat = Zero | OneMoreThan of nat ;;

let two = OneMoreThan (OneMoreThan Zero) ;;
let three = OneMoreThan (OneMoreThan (OneMoreThan Zero)) ;;

let rec add m n =
    match m with
        Zero -> n
      | OneMoreThan m' -> OneMoreThan (add m' n) ;;


let rec int_of_nat = function
    Zero -> 0
  | OneMoreThan n -> 1 + int_of_nat n ;;

int_of_nat two ;;


let rec mul m n =
    match (m, n) with
        (Zero, _) | (_, Zero) -> Zero
      | (_, (OneMoreThan Zero)) -> m
      | (_, (OneMoreThan n')) -> add m (mul m n') ;;

let four = mul two two ;;
let sixteen = mul four four ;;


let rec monus m n =
    match (m, n) with
        (Zero, _) -> Zero
      | (OneMoreThan _, Zero) -> m
      | (OneMoreThan m', OneMoreThan n') -> monus m' n' ;;

monus three two ;;
monus two  four ;;
