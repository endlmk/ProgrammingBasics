open Heap
(* 目的:ヒープソートを行う *)
(* heap_sort : int list -> int list *)
let heap_sort lst = 
    let h = create 100 0 "" in
    let h = List.fold_right (fun elem h -> let (i, h) = insert h elem "" in h) lst h in
    let rec enum_heap i h lst=
        if i = 0 then lst
        else let ((a_min, _), h) = split_top h in
            enum_heap (i - 1) h (lst @ [a_min])
    in enum_heap (List.length lst) h [] 
    
let%test _ = heap_sort [2;5;6;4;9;7;1] = [1;2;4;5;6;7;9]
let%test _ = heap_sort [10;40;22;35;11] = [10;11;22;35;40]