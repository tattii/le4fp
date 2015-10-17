(* Exercise 4.5

以下の関数repeatはdouble,fourtimesなどを一般化したもので,
fをn回,xに適用する関数である.
 
これを使って,フィボナッチ数を計算する関数fibを定義する.
以下の...の部分を埋めよ.
    let fib n =
        let (fibn, _) = ...
        in fibn ;;
*)

let rec repeat f n x =
    if n > 0 then repeat f (n - 1) (f x) else x ;;

let fib n =
    let (fibn, _) = (repeat (fun x ->
        let (prev, curr) = x in (curr, prev + curr)
    ) n (0, 1))
    in fibn
;;

fib 10 ;;
