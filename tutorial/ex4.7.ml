(* Exercise 4.7

s k k が恒等関数として働く理由を
s k k 1 が評価されるステップを示すことで,説明せよ.

また, fun x y -> y と同様に働く関数を,
コンビネータsとkを関数適用のみで組み合わせた形で表現せよ.
   # ( (* s,k *) ) 1 2 ;;
   - : int = 2
*)

let k x y = x ;;
let s x y z = x z (y z) ;;

s k k 1 ;;
(*
s k k 1
k 1 (k 1)
k 1 1
1
*)

(k (s k k)) 1 2 ;;
