(* Exercise 5.6

  quick 関数を @ を使わないように書き換える.
  quicker は未ソートのリスト l と, 
  sorted というソート済みでその要素の最小値が
  l の要素の最大値より大きいようなリストを受け取る.
  定義を完成させよ.
 
  let rec quicker l sorted = ...
*)

let rec quicker l sorted =
    match l with
      [] -> sorted
    | [x] -> x :: sorted
    | x :: xs -> (* x is the pivot *)
        let rec partition left right = function
            [] -> quicker left (quicker right sorted)
          | y :: ys -> if x < y then partition left (y :: right) ys
                       else partition (y :: left) right ys
        in partition [] [x] xs ;;

  let quick l = quicker l [] ;;

  quick [2; 3; -1; 10; -5; 1] ;; 
