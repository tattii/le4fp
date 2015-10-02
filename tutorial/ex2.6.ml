open Printf
let rate_usd_jpy = 111.12 

let jpy_of_usd(d :float) =
  int_of_float( floor(d *. rate_usd_jpy) )
;;

let usd_of_jpy(y :int) =
  float_of_int( truncate( float_of_int(y) /. rate_usd_jpy *. 100.0) ) /. 100.0
;;

let print_jpy_of_usd(d: float) =
  let jpy = int_of_float( floor(d *. rate_usd_jpy) ) in
  Printf.sprintf "%.2f dollars are %d yen." d jpy
;;

jpy_of_usd 2.99 ;;
usd_of_jpy 300 ;;
print_jpy_of_usd 10.00 ;;
