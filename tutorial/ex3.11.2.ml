(* 組み合わせ数(n,m)を再帰的に求める *)
let rec comb (n, m) =
    if m = 0 || n = m then 1
    else comb(n - 1, m) + comb(n - 1, m - 1)
;; 
