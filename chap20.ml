
type color_t = Red | Black

type ('a, 'b) rb_tree_t = Empty 
                          | Node of ('a, 'b) rb_tree_t * 'a * 'b * color_t * ('a, 'b) rb_tree_t

(* 目的:赤黒木をバランスさせる *)
(* balance:rb_tree_t -> rb_tree_t *)
let balance t = match t with
    Node (Node (Node (ta, xk, xv, Red, tb), yk, yv, Red, tc), zk, zv, Black, td) |
    Node (Node (ta, xk, xv, Red, Node (tb, yk, yv, Red, tc)), zk, zv, Black, td) |
    Node (ta, xk, xv, Black, Node (Node (tb, yk, yv, Red, tc), zk, zv, Red, td)) | 
    Node (ta, xk, xv, Black, Node (tb, yk, yv, Red, Node (tc, zk, zv, Red, td))) 
        -> (Node (Node (ta, xk, xv, Black, tb), yk, yv, Red, Node (tc, zk, zv, Black, td)))
  | _ -> t

let tree1 = Node (Node (Node (Empty, "x", 1, Red, Empty), "y", 2, Red, Empty), "z", 3, Black, Empty) 
let tree2 = Node (Node (Empty, "x", 1, Red, Node (Empty, "y", 2, Red, Empty)), "z", 3, Black, Empty) 
let tree3 = Node (Empty, "x", 1, Black, Node (Node (Empty, "y", 2, Red, Empty), "z", 3, Red, Empty)) 
let tree4 = Node (Empty, "x", 1, Black, Node (Empty, "y", 2, Red, Node (Empty, "z", 3, Red, Empty))) 

let tree_balanced = Node (Node (Empty, "x", 1, Black, Empty), "y", 2, Red, Node (Empty, "z", 3, Black, Empty)) 

let tree_balanced2 =  Node (Node (Node (Node (Empty, "a", 1, Red, Empty), "b", 2, Black, Empty), "c", 3, Red, Node (Empty, "d", 4, Black, Empty)), "e", 5, Black, Node (Empty, "f", 6, Black, Empty))

let%test _ = balance tree1 = tree_balanced
let%test _ = balance tree2 = tree_balanced
let%test _ = balance tree3 = tree_balanced
let%test _ = balance tree4 = tree_balanced
let%test _ = balance tree_balanced = tree_balanced
let%test _ = balance tree_balanced2 = tree_balanced2

(* 目的:赤黒木にキーと値を挿入する *)
(* insert: rb_tree_t -> 'a -> 'b -> rb_tree_t *)
let insert t k v = 
    let rec insert_rb t k v = match t with 
        Empty -> Node (Empty, k, v, Red, Empty)
      | Node (t_l, kn, vn, c, t_r) -> 
            if kn = k then Node (t_l, kn, v, c, t_r)
            else if k < kn then balance (Node ((insert_rb t_l k v), kn, vn, c, t_r))
            else balance (Node (t_l, kn, vn, c, (insert_rb t_r k v)))
    in let t_i = insert_rb t k v in
    match t_i with
        Empty -> assert false
      | Node (Empty, kn, vn, Red, Empty) -> Node (Empty, kn, vn, Black, Empty)
      | Node (Node(t1,kl,vl,Red,tlr), kn, vn, Red, t_r) -> Node (Node(t1,kl,vl,Red,tlr), kn, vn, Black, t_r)
      | Node (t_l, kn, vn, Red, Node(trl,kr,vr,Red,trr)) -> Node (t_l, kn, vn, Black, Node(trl,kr,vr,Red,trr))
      | _ -> t_i

let tree_s1 = Node (Empty, "x", 1, Black, Empty)
let tree_s2 = Node (Empty, "x", 1, Black, Node (Empty, "y", 2, Red, Empty))
let tree_s3 = Node (Node (Empty, "x", 1, Black, Empty), "y", 2, Red, Node (Empty, "z", 3, Black, Empty))

let%test _ = insert Empty "x" 1 = tree_s1
let%test _ = insert tree_s1 "y" 2 = tree_s2
let%test _ = insert tree_s2 "z" 3 = tree_s3