(* 時給 *)
let jikyu = 950

(* 基本給 *)
let kihonkyu = 100

(* 目的:働いた時間 x に応じたアルバイト代を計算する *)
(* kyuyo : int -> int *)
let kyuyo x =  x * jikyu + kihonkyu

let%test _ = kyuyo 25 = 23850
let%test _ = kyuyo 28 = 26700
let%test _ = kyuyo 31 = 29550

(* 目的: BMIを計算する *)
(* bmi: float -> float -> float *)
let bmi l w = w /. l ** 2.0

let%test _ = bmi 1. 30. = 30.
let%test _ = bmi 1.5 45. = 20.
let%test _ = bmi 2. 60. = 15.

(* 目的:鶴の数から足の本数を計算する *)
(* tsuru_no_ashi: int -> int *)
let tsuru_no_ashi x = x * 2

let%test _ = tsuru_no_ashi 3 = 6
let%test _ = tsuru_no_ashi 6 = 12
let%test _ = tsuru_no_ashi 10 = 20

(* 目的:亀の数から足の本数を計算する *)
(* kame_no_ashi: int -> int *)
let kame_no_ashi x = x * 4

let%test _ = kame_no_ashi 2 = 8
let%test _ = kame_no_ashi 5 = 20
let%test _ = kame_no_ashi 9 = 36

(* 目的:鶴の数と亀の数から足の本数を計算する *)
(* tsurukame_no_ashi: int -> int -> int *)
let tsurukame_no_ashi x y = tsuru_no_ashi x + kame_no_ashi y

let%test _ = tsurukame_no_ashi 3 6 = 30
let%test _ = tsurukame_no_ashi 5 2 = 18
let%test _ = tsurukame_no_ashi 7 4 = 30

(* 鶴と亀の合計と足の数から鶴の数を計算する *)
(* tsurukame: int -> int -> int *)
let tsurukame sum ashi  = (4 * sum - ashi) / 2 

let%test _ = tsurukame 9 30 = 3
let%test _ = tsurukame 7 18 = 5
let%test _ = tsurukame 11 30 = 7