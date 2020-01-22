
(* 文字列を受け取ったら、その文字列に関数が呼ばれた回数をつけた文字列を返す *)
(* gensym : string -> string *)
let count = ref (-1)
let gensym s = 
    (count := !count + 1;
     s ^ (string_of_int !count))

let%test _ = gensym "a" = "a0"
let%test _ = gensym "a" = "a1"
let%test _ = gensym "x" = "x2"

(* 配列からそこにフィボナッチ数を入れた配列を返す *)
(* fib_array : int array -> int array *)
let fib_array a = 
    let rec fib_loop i a = 
        if i = 0 then (a.(i) <- 0; fib_loop (i + 1) a) 
        else if i = 1 then (a.(i) <- 1; fib_loop (i + 1) a)
        else if i < Array.length a then (a.(i) <- a.(i - 2) + a.(i - 1); fib_loop (i + 1) a)
        else a
    in fib_loop 0 a

let%test _ = fib_array [|0;0;0;0;0|] = [|0;1;1;2;3|]
let%test _ = fib_array [|0;0;0;0;0;0;0;0;0;0|] = [|0;1;1;2;3;5;8;13;21;34|]