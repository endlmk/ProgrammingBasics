open Chap17
open Tree

(* 目的:駅名2つとekikan_tree_tから2つの駅間距離を返す *)
(* get_ekikan_kyori: string -> string -> ekikan_tree_t -> float *)
let get_ekikan_kyori3 e_f e_t t = assoc e_t (search t e_f)
let ekikan_tree = let t1 = insert empty "代々木公園" [("明治神宮前", 1.2);("代々木上原", 1.0)] in
                  let t2 = insert t1 "代々木上原" [("代々木公園", 1.0)] in
                  insert t2 "明治神宮前" [("代々木公園", 1.2)]

let%test _ = get_ekikan_kyori3 "代々木公園" "代々木上原" ekikan_tree = 1.0
let%test _ = get_ekikan_kyori3 "代々木公園" "明治神宮前" ekikan_tree = 1.2
let%test _ = (try get_ekikan_kyori3 "代々木上原" "明治神宮前" ekikan_tree with Not_found -> infinity) = infinity
