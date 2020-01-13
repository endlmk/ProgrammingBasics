open Chap9
open Chap10
open Chap12
open Chap14
open Chap15
open Chap16

type nengou_t = Meiji of int (* 明治 *)
              | Taisho of int (* 大正 *)
              | Showa of int (* 昭和 *)
              | Heisei of int (* 平成 *)

(* 目的: 誕生年と現在の年をnengou_tの型として受け取って年齢を返す *)
(* nenrei: nengou_t -> nengou_t-> int *)
let to_seireki nengou = match nengou with 
    Meiji (n) -> n + 1867
  | Taisho (n) -> n + 1911
  | Showa (n) -> n +1925
  | Heisei (n) -> n +1988

let nenrei nb nn = (to_seireki nn) - (to_seireki nb)

let%test _ = nenrei (Showa(61)) (Heisei(31)) = 33
let%test _ = nenrei (Taisho(10)) (Heisei(2)) = 69
let%test _ = nenrei (Meiji(40)) (Showa(30)) = 48

type year_t = January of int 
            | February of int
            | March of int
            | April of int
            | May of int
            | June of int
            | July of int
            | August of int
            | September of int 
            | October of int
            | November of int
            | December of int

type seiza_t = Areis
             | Taurus
             | Gemini
             | Cancer
             | Leo
             | Virgo
             | Libra
             | Scorpius
             | Sagittarius
             | Capricornus
             | Aquarius
             | Pisces

(* 目的: year_tからseiza_tを返す *)
(* seiza: year_t -> seiza_t *)
let seiza y = match y with 
    January (d) -> if d <= 19 then Capricornus else Aquarius
  | February (d) -> if d <= 18 then Aquarius else Pisces
  | March (d) -> if d <= 20 then Pisces else Areis
  | April (d) -> if d <= 19 then Areis else Taurus
  | May (d) -> if d <= 20 then Taurus else Gemini
  | June (d) -> if d <= 21 then Gemini else Cancer
  | July (d) -> if d <= 22 then Cancer else Leo
  | August (d) -> if d <= 22 then Leo else Virgo
  | September (d) -> if d <= 22 then Virgo else Libra
  | October (d) -> if d <= 23 then Libra else Scorpius
  | November (d) -> if d <= 22 then Scorpius else Sagittarius
  | December (d) -> if d <= 21 then Sagittarius else Capricornus

let%test _ = seiza (April(5)) = Areis
let%test _ = seiza (June(26)) = Cancer
let%test _ = seiza (September(14)) = Virgo

type tree_t = Empty
            | Leaf of int
            | Node of tree_t * int * tree_t

let tree1 = Empty
let tree2 = Leaf(3)
let tree3 = Node (tree1, 4, tree2)
let tree4 = Node (tree2, 5, tree3)

(* 目的:tree_tのすべての値を2倍にした木を返す *)
(* tree_double: tree_t -> tree_t *)
let rec tree_double t = match t with
    Empty -> Empty
  | Leaf(n) -> Leaf(2* n)
  | Node(t1, n, t2) -> Node((tree_double t1), 2 * n, (tree_double t2))

let%test _ = tree_double tree2 = Leaf(6)
let%test _ = tree_double tree3 = Node (Empty, 8, Leaf(6))
let%test _ = tree_double tree4 = Node (Leaf(6), 10, Node (Empty, 8, Leaf(6)))

(* 目的:tree_tのすべての値に関数fを適用した木を返す *)
(* tree_map -> (int -> int) -> tree_t -> tree_t *)
let rec tree_map f t = match t with
    Empty -> Empty
  | Leaf(n) -> Leaf (f n)
  | Node(t1, n, t2) -> Node((tree_map f t1), (f n), (tree_map f t2))

let%test _ = tree_map (fun n -> (+) 2 n) tree2 = Leaf(5)
let%test _ = tree_map (fun n -> ( * ) 2 n) tree3 = Node (Empty, 8, Leaf(6))
let%test _ = tree_map (fun n -> (-) n 2) tree4 = Node (Leaf(1), 3, Node (Empty, 2, Leaf(1)))

(* 目的: tree_tの節と葉が合計いくつあるかを返す *)
(* tree_length: tree_t -> int *)
let rec tree_length t = match t with
    Empty -> 0
  | Leaf(n) -> 1
  | Node(t1, n, t2) -> (tree_length t1) + (tree_length t2) + 1

let%test _ = tree_length tree2 = 1
let%test _ = tree_length tree3 = 2
let%test _ = tree_length tree4 = 4

(* 目的: tree_tの深さを返す *)
(* tree_depth = tree_t -> int *)
let rec tree_depth t = match t with
    Empty -> 0
  | Leaf(n) -> 0
  | Node(t1, n, t2) -> (max (tree_depth t1) (tree_depth t2)) + 1

let%test _ = tree_depth tree1 = 0
let%test _ = tree_depth tree2 = 0
let%test _ = tree_depth tree3 = 1
let%test _ = tree_depth tree4 = 2

type 'a tree_a_t = Empty 
               | Leaf of 'a
               | Node of 'a tree_a_t * 'a * 'a tree_a_t

let tree_a_1 = Empty
let tree_a_2 = Leaf(3)
let tree_a_3 = Node (tree_a_1, 4, tree_a_2)
let tree_a_4 = Node (tree_a_2, 5, tree_a_3)

let rec sum_tree tree = match tree with
    Empty -> 0
  | Leaf (a) -> a
  | Node (t1, a, t2) -> (sum_tree t1) + a + (sum_tree t2)


let%test _ = sum_tree tree_a_1 = 0
let%test _ = sum_tree tree_a_2 = 3
let%test _ = sum_tree tree_a_3 = 7
let%test _ = sum_tree tree_a_4 = 15

type ekikan_tree_t = Empty
                   | Node of ekikan_tree_t * (string * (string * float) list) * ekikan_tree_t

(* 目的: 駅名と駅名の距離の組のリストからその駅までの距離を返す *)
(* assoc: string -> (string * float) list -> float *)
let rec assoc s lst = match lst with 
    [] -> infinity
  | fst::rest -> let (s_l, l_l) = fst in
                 if s_l = s then l_l else assoc s rest

let%test _ = assoc "後楽園" [("新大塚", 1.2); ("後楽園", 1.8)] = 1.8
let%test _ = assoc "池袋" [("新大塚", 1.2); ("後楽園", 1.8)] = infinity

(* 目的; ekikan_tree_tとekikan_tを受け取り、木に挿入する *)
(* insert_ekikan: ekikan_tree_t -> ekikan_t -> ekikan_tree_t *)
let rec insert_ekikan_single t eki_name eki_to kyori = match t with
    Empty -> Node (Empty, (eki_name, [(eki_to, kyori)]), Empty)
  | Node(t1, (e_n, e_l), t2) -> 
        if e_n = eki_name then Node(t1, (e_n, (eki_to, kyori)::e_l), t2)
        else if eki_name < e_n then Node((insert_ekikan_single t1 eki_name eki_to kyori), (e_n, e_l), t2)
        else Node(t1, (e_n, e_l), (insert_ekikan_single t2 eki_name eki_to kyori))

let insert_ekikan t e = let {kiten=k;shuten=s;keiyu=_;kyori=ky;jikan=_;} = e in
    let t1 = insert_ekikan_single t k s ky in
    insert_ekikan_single t1 s k ky

let ekikan1 = {kiten="代々木上原";shuten="代々木公園";keiyu="千代田線";kyori=1.0;jikan=2}
let ekikan2 = {kiten="代々木公園"; shuten="明治神宮前"; keiyu="千代田線"; kyori=1.2; jikan=2} 
let ekikan_tree1 = Node (Empty, ("代々木上原", [("代々木公園", 1.0)]), (Node (Empty, ("代々木公園", [("代々木上原", 1.0)]), Empty)))
let ekikan_tree2 = Node (Empty, ("代々木上原", [("代々木公園", 1.0)]), (Node (Empty, ("代々木公園", [("明治神宮前", 1.2);("代々木上原", 1.0)]), Node (Empty, ("明治神宮前", [("代々木公園", 1.2)]), Empty))))
let%test _ = insert_ekikan Empty ekikan1 = ekikan_tree1
let%test _ = insert_ekikan ekikan_tree1 ekikan2 = ekikan_tree2

(* 目的: ekikan_tree_tにekikan_t listの駅間をすべて挿入した木を返す *)
(* inserts_ekikan: ekikan_tree_t -> ekikan_t list -> ekikan_tree_t *)
let inserts_ekikan t l = List.fold_right (fun e t -> insert_ekikan t e) l t

let%test _ = inserts_ekikan Empty [ekikan2; ekikan1] = ekikan_tree2

(* 目的:駅名2つとekikan_tree_tから2つの駅間距離を返す *)
(* get_ekikan_kyori: string -> string -> ekikan_tree_t -> float *)
let rec get_ekikan_kyori e_f e_t t = match t with
    Empty -> infinity
  | Node(t1, (e_n, e_l), t2) -> if e_n = e_f then assoc e_t e_l
                                else if e_f < e_n then get_ekikan_kyori e_f e_t t1
                                else get_ekikan_kyori e_f e_t t2

let%test _ = get_ekikan_kyori "代々木公園" "代々木上原" ekikan_tree2 = 1.0
let%test _ = get_ekikan_kyori "代々木公園" "明治神宮前" ekikan_tree2 = 1.2
let%test _ = get_ekikan_kyori "代々木上原" "明治神宮前" ekikan_tree2 = infinity

(* 目的: 直前に確定した駅pと未確定の駅のリストv,駅間距離リストkから更新処理を行ったリストを返す *)
(* koushin: eki_t -> eki_t list -> ekikan_t list -> eki_t list *)
let koushin2 p lst lst_k = 
    let ekikan_tree = inserts_ekikan Empty lst_k in 
    let update_eki p v = 
        let k = get_ekikan_kyori p.namae v.namae ekikan_tree in
        if (p.saitan_kyori +. k) < v.saitan_kyori 
            then {namae=v.namae;saitan_kyori=(p.saitan_kyori +. k);temae_list=v.namae::p.temae_list}
            else v
    in List.map (fun e -> update_eki p e) lst  

let%test _ = koushin2
    {namae="代々木上原"; saitan_kyori=0.0; temae_list=["代々木上原"];}
    [ {namae="代々木公園"; saitan_kyori=infinity; temae_list=[];}; 
      {namae="明治神宮前"; saitan_kyori=infinity; temae_list=[];}; ]
      global_ekikan_list = 
    [ {namae="代々木公園"; saitan_kyori=1.0; temae_list=["代々木公園"; "代々木上原"];}; 
      {namae="明治神宮前"; saitan_kyori=infinity; temae_list=[];}; ]

(* 目的: eki_tの未確定リストとekikan_tのリストから、ダイクストラのアルゴリズムに従って、最短距離と最短経路が入ったリストを返す *)
(* dijkstra_main eki_t list -> ekikan_t list -> eki_t list  *)
let rec dijkstra_main2 lst lst_k = match lst with 
    [] -> []
    | _ -> let (p, undet) = saitan_wo_bunri2 lst in
        p::(dijkstra_main2 (koushin2 p undet lst_k) lst_k)

let%test _ = dijkstra_main2
    [
        {namae="代々木上原"; saitan_kyori=0.0; temae_list=["代々木上原"];}; 
        {namae="代々木公園"; saitan_kyori=infinity; temae_list=[];}; 
        {namae="明治神宮前"; saitan_kyori=infinity; temae_list=[];}; 
        {namae="表参道"; saitan_kyori=infinity; temae_list=[];};
    ]
    global_ekikan_list
    =
    [
        {namae="代々木上原"; saitan_kyori=0.0; temae_list=["代々木上原"];}; 
        {namae="代々木公園"; saitan_kyori=1.0; temae_list=["代々木公園";"代々木上原"];}; 
        {namae="明治神宮前"; saitan_kyori=2.2; temae_list=["明治神宮前";"代々木公園";"代々木上原"];}; 
        {namae="表参道"; saitan_kyori=3.1; temae_list=["表参道";"明治神宮前";"代々木公園";"代々木上原"];};        
    ]

(* 目的: 始点の駅名(ローマ字)と終点の駅名(ローマ字)から最短経路が入った終点の駅のレコードを返す *)
(* dijkstra: string -> string -> eki_t *)
let dijkstra2 siten syuten = 
    let sorted = seiretsu global_ekimei_list in
    let siten_k = romaji_to_kanji siten sorted in 
    let syuten_k = romaji_to_kanji syuten sorted in 
    let init_eki = make_initiak_eki_list sorted siten_k in
    let result = dijkstra_main2 init_eki global_ekikan_list in 
    let rec find s lst = match lst with
        [] -> {namae=""; saitan_kyori=infinity; temae_list=[];}        
      | fst::rest -> if fst.namae = s then fst else find s rest
    in find syuten_k result

let%test _ = dijkstra2 "yoyogiuehara" "yoyogikouen" = {namae="代々木公園"; saitan_kyori=1.0; temae_list=["代々木公園";"代々木上原"];} 
let%test _ = dijkstra2 "omotesandou" "yoyogiuehara" = {namae="代々木上原"; saitan_kyori=3.1; temae_list=["代々木上原";"代々木公園";"明治神宮前";"表参道"];} 
let%test _ = dijkstra2 "myogadani" "korakuen" = {namae = "後楽園"; saitan_kyori = 1.8; temae_list = ["後楽園"; "茗荷谷"];}

let saitan_wo_bunri3 fst rest = 
    let update_min = fun e ((p_e:eki_t), (p_l:eki_t list)) -> 
        let {namae=_; saitan_kyori=s_e; temae_list=_;} = e in 
        let {namae=_; saitan_kyori=s_p; temae_list=_;} = p_e in
        if p_e.namae = "" then (e, [])
        else if s_e <= s_p 
            then (e, p_e::p_l)
            else (p_e, e::p_l) in
    List.fold_right update_min rest (fst, [])

let%test _ = saitan_wo_bunri3
    {namae="代々木公園"; saitan_kyori=1.0; temae_list=["代々木上原"];}
    [ 
        {namae="明治神宮前"; saitan_kyori=0.9; temae_list=["表参道"];}; 
        {namae="乃木坂"; saitan_kyori=1.1; temae_list=["赤坂"];};
    ]
    = (
        {namae="明治神宮前"; saitan_kyori=0.9; temae_list=["表参道"];},
        [
            {namae="代々木公園"; saitan_kyori=1.0; temae_list=["代々木上原"];}; 
            {namae="乃木坂"; saitan_kyori=1.1; temae_list=["赤坂"];};
        ]     
    )

let minimum fst rest =   
    List.fold_right (fun l r -> if l < r then l else r) rest fst

let%test _ = minimum 3 [] = 3
let%test _ = minimum 1 [2] = 1
let%test _ = minimum 3 [2] = 2
let%test _ = minimum 3 [2;6;4;1;8] = 1
