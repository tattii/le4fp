float_of_int 3 +. 2.5 ;; (* float = 5.5 *)

int_of_float 0.7 ;; (* int = 0 *)

if "11" > "100" then "foo" else "bar" ;; (* string = "foo" *)

char_of_int ((int_of_char 'A') + 20) ;; (* char = 'U' *)

int_of_string "0xff" ;; (* int = 255 *)

5.0 ** 2.0 ;; (* float = 25. *) 
