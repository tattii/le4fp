(* Exercise 7.8
  以下の関数 charge はお金を"くずす"関数である.

  let rec charge = function
      (_, 0) -> []
    | ((c :: rest) as coins, total) ->
        if c > total then charge (rest, total)
        else c :: charge (coins, total - c) ;;

  しかし, この定義は先頭にあるコインを出来る限り使おうとするため, 
  可能なコインの組み合わせがあるときにでも失敗してしまうことがある.

   # charge ([5; 2], 16) ;;
   Exception: ....

  これを, 例外処理を用いて解がある場合には出力するようにしたい.
  以下のプログラムの, 2箇所の...部分を埋め, プログラムの説明を行え.

   let rec charge = function
       (_, 0) -> []
     | ((c :: rest) as coins, total) ->
         if c > total then charge (rest, total)
         else 
           (try
             c :: charge (coins, total - c)
            with Failure "charge" -> ...)
     | _ -> ... ;;
*)
 
let rec charge = function
    (_, 0) -> []
  | ((c :: rest) as coins, total) ->
      if c > total then charge (rest, total)
      else 
        (try
           c :: charge (coins, total - c)
         with Failure "charge" -> charge(rest, total))
  | _ -> raise (Failure "charge") ;;



