open String

(* ASCIIコードが最も大きい文字を返す *)
let max_ascii (str) = 
    let rec max_ascii_iter (n, max) =
        if n >= length(str)  then char_of_int(max)
        else if int_of_char(str.[n]) > max
            then max_ascii_iter(n + 1, int_of_char(str.[n]))
            else max_ascii_iter(n + 1, max)
    in max_ascii_iter(0, 0)
;;

max_ascii "OCaml" ;;
