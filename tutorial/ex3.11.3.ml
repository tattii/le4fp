(* 末尾再帰的関数を使ってフィボナッチ数を求める *)
let fib(n) =
    let rec fib_iter(n, x, y) =
        if n = 1 then y
        else fib_iter(n - 1, y, x + y)
    in fib_iter(n, 0, 1)
;;

fib 50 ;;
