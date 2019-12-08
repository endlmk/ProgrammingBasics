open Chap4

(* 目的: 時間を受け取って午前か午後かを返す *)
(* jikan: int -> string *)
let jikan x = if x <= 12 then "午前" else "午後"

let%test _ = jikan 11 = "午前"
let%test _ = jikan 12 = "午前"
let%test _ = jikan 13 = "午後"

(* 目的: 月と日を受け取ったら星座を返す *)
(* seiza: int -> int -> string  *)

(* 目的: ２次方程式の判別式の値を返す *)
(* hanbetsushiki: fkloat -> float -> float -> float *)
let hanbetsushiki a b c = b *. b -. 4.0 *. a *. c

let%test _ = hanbetsushiki 1.0 2.0 1.0 = 0.0
let%test _ = hanbetsushiki 4.0 2.0 4.0 = -60.0
let%test _ = hanbetsushiki 1.0 7.0 12.0 = 1.0

(* 目的: ２次方程式の解の個数を返す *)
(* kai_no_kosuu: float -> float -> float -> int *)
let kai_no_kosuu a b c = if hanbetsushiki a b c < 0.0 then 0 else
                         if hanbetsushiki a b c > 0.0 then 2 else 1

let%test _ = kai_no_kosuu 1.0 2.0 1.0 = 1
let%test _ = kai_no_kosuu 4.0 2.0 4.0 = 0
let%test _ = kai_no_kosuu 1.0 7.0 12.0 = 2

(* 目的: ２次方程式が虚数解を持つかどうかを返す *)
(* kyousuukai: float -> float -> float -> bool *)
let kyosuukai a b c = kai_no_kosuu a b c = 0

let%test _ = kyosuukai 1.0 2.0 1.0 = false
let%test _ = kyosuukai 4.0 2.0 4.0 = true
let%test _ = kyosuukai 1.0 7.0 12.0 = false

(* 目的: BMIから体型を返す *)
(* taikei: float -> float -> string *)
let taikei l w = if bmi l w < 18.5 then "やせ"
                    else if bmi l w >= 18.5 && bmi l w < 25.0 then "標準"
                    else if bmi l w >= 25.0 && bmi l w < 30.0 then "肥満"
                    else "高度肥満"

let%test _ = taikei 1. 30. = "高度肥満"
let%test _ = taikei 1.5 63. = "肥満"
let%test _ = taikei 1.5 45. = "標準"
let%test _ = taikei 2. 60. = "やせ"
