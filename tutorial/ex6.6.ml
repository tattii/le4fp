(* Exercise 6.6
  二分木も左右を反転させた木を返す関数 reflect を定義せよ. 

  また, 任意の二分木 t に対して成立する, 以下の方程式を完成させよ.

     preorder(reflect(t)) = ?
      inorder(reflect(t)) = ?
    postorder(reflect(t)) = ?
*)


type 'a tree = Lf | Br of 'a * 'a tree * 'a tree ;;

let comptree = Br(1, Br(2, Br(4, Lf, Lf),
                           Br(5, Lf, Lf)),
                     Br(3, Br(6, Lf, Lf),
                           Br(7, Lf, Lf))) ;;

let rec reflect = function
    Lf -> Lf
  | Br(x, left, right) -> Br(x, reflect right, reflect left) ;;

reflect comptree ;;



let rec preorder = function
    Lf -> []
  | Br (x, left, right) -> x :: (preorder left) @ (preorder right) ;;

let rec inorder = function
    Lf -> []
  | Br (x, left, right) -> (inorder left) @ (x :: inorder right) ;;

let rec postorder = function
    Lf -> []
  | Br (x, left, right) -> (postorder left) @ (postorder right) @ [x] ;;

let rec reverse = function
    [] -> []
  | x :: rest -> (reverse rest) @ [x] ;;

(* preorder(reflect(t)) = reverse(postorder(t))*)
preorder(reflect comptree) ;;
reverse(postorder comptree) ;;

(* inorder(reflect(t)) = reverse(inorder(t))*)
inorder(reflect comptree) ;;
reverse(inorder comptree) ;;

(* postorder(reflect(t)) = reverse(preorder(t))*)
postorder(reflect comptree) ;;
reverse(preorder comptree) ;;
