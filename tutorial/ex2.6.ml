let rate_usd_jpy = 111.12 

let jpy_of_usd(d :float) =
  int_of_float( floor(d *. rate_usd_jpy) )

let usd_of_jpy(y :int) =
  float_of_int( truncate( float_of_int(y) /. rate_usd_jpy *. 100.0) ) /. 100.0

let print_jpy_of_usd(d: float) =
    
