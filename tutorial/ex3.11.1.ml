(* Euclidの互除法で2整数の最大公約数を求める *)
let rec gcd (m, n) =
    if n mod m = 0 then m
    else gcd(n mod m, m)
;;

gcd(2, 3) ;;
gcd(144, 108) ;;
