let rec pow (x, n) =
    if n = 0 then 1.0 else x *. pow(x, (n - 1))
;;

(* 再帰がlog2_n ですむ定義 *)
let rec powb (x, n) =
    if n = 0 then 1.0
    else if n mod 2 = 0 then powb(x ** 2.0, n / 2)
    else powb(x ** 2.0, (n - 1) / 2) *. x
;;

pow(2.0, 10) ;;
powb(2.0, 10) ;;
