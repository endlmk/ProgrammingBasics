
type index_t = int ref

type ('a, 'b) t = int ref * (index_t * 'a * 'b) array

let create n a b = (ref 0, Array.make n ((ref (-1) : index_t), a, b))

let parent_index c = (c - 1) / 2 
let%test _ = parent_index 2 = 0
let%test _ = parent_index 9 = 4
let%test _ = parent_index 11 = 5

let children_index p = (2 * p + 1 , 2 * p + 2)
let%test _ = children_index 0 = (1, 2)
let%test _ = children_index 4 = (9, 10)

let insert (h : ('a, 'b) t) a b = 
    let (n, arr) = h in
    let i = ref !n in
    let rec swap_parent i arr = 
        if i = 0 then arr 
        else let p = parent_index i in 
            let (ic, ac, bc) = arr.(i) in
            let (ip, ap, bp) = arr.(p) in
            if ac < ap then (
                ic := p;
                ip := i;
                arr.(i) <- (ip, ap, bp);
                arr.(p) <- (ic, ac, bc);
                swap_parent p arr
            )
            else arr 
    in 
    (arr.(!n) <- (i, a, b);
     n := !n + 1;
     (i, (n, swap_parent !i arr)))

let get (h : ('a, 'b) t) (i : index_t) = 
    let (n, arr) = h in
    let (j, a, b) = arr.(!i) in
    (a, b)

let set (h : ('a, 'b) t) (i : index_t) a b = 
    let (n, arr) = h in
    let rec swap_children i arr = 
        let (c1, c2) = children_index i in
        if c1 > (!n - 1) then arr
        else if c1 = (!n - 1) then
            let (ip, ap, bp) = arr.(i) in
            let (ic1, ac1, bc1) = arr.(c1) in
            if ap > ac1 then (
                ic1 := i;
                ip := c1;
                arr.(c1) <- (ip, ap, bp);
                arr.(i) <- (ic1, ac1, bc1);
                arr
            )
            else arr
        else let (ip, ap, bp) = arr.(i) in
            let (ic1, ac1, bc1) = arr.(c1) in
            let (ic2, ac2, bc2) = arr.(c2) in
            if (ap <= ac1) && (ap <= ac2) then arr 
            else (
                let (ics, acs, bcs) = if ac1 < ac2 then (ic1, ac1, bc1) else (ic2, ac2, bc2) in
                let ic = !ics in
                ics := i;
                ip := ic;
                arr.(ic) <- (ip, ap, bp);
                arr.(i) <- (ics, acs, bcs);
                swap_children ic arr
            )
    in
    arr.(!i) <- (i, a, b);
    (n, swap_children !i arr)

let split_top (h : ('a, 'b) t) = 
   let (n, arr) = h in
   let rec swap_children i arr = 
        let (c1, c2) = children_index i in
        if c1 > (!n - 1) then arr
        else if c1 = (!n - 1) then
            let (ip, ap, bp) = arr.(i) in
            let (ic1, ac1, bc1) = arr.(c1) in
            if ap > ac1 then (
                ic1 := i;
                ip := c1;
                arr.(c1) <- (ip, ap, bp);
                arr.(i) <- (ic1, ac1, bc1);
                arr
            )
            else arr
        else let (ip, ap, bp) = arr.(i) in
            let (ic1, ac1, bc1) = arr.(c1) in
            let (ic2, ac2, bc2) = arr.(c2) in
            if (ap <= ac1) && (ap <= ac2) then arr 
            else (
                let (ics, acs, bcs) = if ac1 < ac2 then (ic1, ac1, bc1) else (ic2, ac2, bc2) in
                let ic = !ics in
                ics := i;
                ip := ic;
                arr.(ic) <- (ip, ap, bp);
                arr.(i) <- (ics, acs, bcs);
                swap_children ic arr
            )
    in
    let (i0, a0, b0) = arr.(0) in
    let (i, a, b) = arr.(!n - 1) in
    arr.(0) <- (i, a, b);
    n := !n - 1;
    ((a0, b0), (n, swap_children 0 arr))

let h = create 10 0 "" 
let (index1, h) = insert h 3 "test"
let%test _ = get h index1 = (3, "test")
let (index2, h) = insert h 2 "test"
let%test _ = !index1 = 1
let%test _ = !index2 = 0
let%test _ = get h index2 = (2, "test")
let h = set h index2 4 "test"
let%test _ = !index1 = 0
let%test _ = !index2 = 1
let (index3, h) = insert h 1 "test"
let%test _ = !index1 = 2
let%test _ = !index2 = 1
let%test _ = !index3 = 0
let h = set h index3 5 "test"
let%test _ = !index1 = 0
let%test _ = !index2 = 1
let%test _ = !index3 = 2
let ((a_min, b_min), h) = split_top h
let%test _ = (a_min, b_min) = (3, "test")