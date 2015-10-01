(*
 * 1. 型エラー
 *    then int と else unit の違い
 *)
if true&&false then 2 ;;


(*
 * 2. 文法エラー
 *    *- という演算子がない
 *    8 * -2 とすうればよい
 *)
8*-2 ;;

(*
 * 3. 例外
 *    0xfg という数値が存在しない
 *)
int_of_string "0xfg"

(*
 * 4. 型エラー
 *    float を int として扱おうとしている
 *)
int_of_float -0.7
