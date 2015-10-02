let rec sigmaf (f, n) =
    if n = 0 then 0.0 else f n +. sigmaf (f, n-1)
;;

let trapezoidal_rule f a d = fun i ->
    (f(a +. float_of_int(i - 1) *. d) +. f(a +. float_of_int(i)*.d)) *. d /. 2.0
;;

let integral f a b =
    let n = 10000 in
	let d = (b -. a) /. float_of_int(n) in
    sigmaf((trapezoidal_rule f a d), n)
;;

integral sin 0.0 3.14159 ;; (*=> 2.0 *)
