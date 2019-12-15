(* 目的: 0から受け取った自然数までの二乗和を求める *)
(* sum_of_square: int -> int *)
let rec sum_of_square n = 
    if n = 0 then 0
    else n * n + sum_of_square (n-1)

let%test _ = sum_of_square 4 = 30
let%test _ = sum_of_square 5 = 55
let%test _ = sum_of_square 6 = 91

(* 数列の第n項を求める *)
(* suuretsu: int -> int *)
let rec suuretsu n = 
    if n = 0 then 3
    else 2 * suuretsu (n-1) -1

let%test _ = suuretsu 0 = 3
let%test _ = suuretsu 3 = 17
let%test _ = suuretsu 4 = 33