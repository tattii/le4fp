(* Exercise 7.8
  以下の関数 charge はお金を"くずす"関数である.

  let rec change = function
      (_, 0) -> []
    | ((c :: rest) as coins, total) ->
        if c > total then change (rest, total)
        else c :: change (coins, total - c) ;;

  しかし, この定義は先頭にあるコインを出来る限り使おうとするため, 
  可能なコインの組み合わせがあるときにでも失敗してしまうことがある.

   # change ([5; 2], 16) ;;
   Exception: ....

  これを, 例外処理を用いて解がある場合には出力するようにしたい.
  以下のプログラムの, 2箇所の...部分を埋め, プログラムの説明を行え.

   let rec change = function
       (_, 0) -> []
     | ((c :: rest) as coins, total) ->
         if c > total then change (rest, total)
         else 
           (try
             c :: charge (coins, total - c)
            with Failure "change" -> ...)
     | _ -> ... ;;
*)
 
let rec change = function
    (_, 0) -> []
  | ((c :: rest) as coins, total) ->
      if c > total then change (rest, total)
      else 
        (try
           c :: change (coins, total - c)
         with Failure "change" -> change(rest, total))
  | _ -> raise (Failure "change") ;;


change ([5; 2], 16) ;;
