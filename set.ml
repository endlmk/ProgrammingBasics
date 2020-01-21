type 'a t = 'a list

let empty = []

let singleton e = [e]

let mem e s = List.mem e s

let union s1 s2 = List.fold_left (fun s elem -> if mem elem s then s else elem::s) s1 s2

let inter s1 s2 = List.filter (fun e -> mem e s2) s1

let diff s1 s2 = List.filter (fun e -> not (mem e s2)) s1

let s23 = union (singleton 2) (singleton 3)

let%test _ = empty = []
let%test _ = singleton 2 = [2]
let%test _ = union (singleton 2) (singleton 3) = [3;2]
let%test _ = diff s23 (singleton 2) = singleton 3
let%test _ = inter s23 (singleton 3) = singleton 3