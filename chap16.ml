open Chap9
open Chap10
open Chap12
open Chap14
open Chap15

(* 目的: 整数のリストから、それまでの数の合計からなるリストを返す *)
(* sum_list: int list -> int list *)
let sum_list lst = 
    let rec hojo lst sum = match lst with
        [] -> []
      | fst::rest -> (sum+fst)::(hojo rest (sum+fst))
    in hojo lst 0

let%test _ = sum_list [3;2;1;4] = [3;5;6;10]
let%test _ = sum_list [3;2;4;6] = [3;5;9;15]
let%test _ = sum_list [1;] = [1;]

(* 目的: 関数と初期値、リストを受け取り初期値から初めてリストの要素を左から順に関数に施し込む *)
(* fold_left: ('a -> 'b -> 'a) -> a' -> b' list -> a' *)
let fold_left f init lst = 
    let rec foldl f lst result = match lst with
        [] -> result
      | fst::rest -> foldl f rest (f result fst)
    in foldl f lst init

let%test _ = fold_left (+) 0 [1;2;3] = 6
let%test _ = fold_left (fun a b -> a - b) 0 [1;2;3] = -6

(* 目的: 直前に確定した駅pと未確定の駅のリストv,駅間距離リストkから更新処理を行ったリストを返す *)
(* koushin: eki_t -> eki_t list -> ekikan_t list -> eki_t list *)
let koushin p lst lst_k = 
    let update_eki p v lst_k = 
        let k = get_ekikan_kyori p.namae v.namae lst_k in
        if (p.saitan_kyori +. k) < v.saitan_kyori 
            then {namae=v.namae;saitan_kyori=(p.saitan_kyori +. k);temae_list=v.namae::p.temae_list}
            else v
    in List.map (fun e -> update_eki p e lst_k) lst  

let%test _ = koushin
    {namae="代々木上原"; saitan_kyori=0.0; temae_list=["代々木上原"];}
    [ {namae="代々木公園"; saitan_kyori=infinity; temae_list=[];}; 
      {namae="明治神宮前"; saitan_kyori=infinity; temae_list=[];}; ]
      global_ekikan_list = 
    [ {namae="代々木公園"; saitan_kyori=1.0; temae_list=["代々木公園"; "代々木上原"];}; 
      {namae="明治神宮前"; saitan_kyori=infinity; temae_list=[];}; ]

(* 目的: eki_tの未確定リストとekikan_tのリストから、ダイクストラのアルゴリズムに従って、最短距離と最短経路が入ったリストを返す *)
(* dijkstra_main eki_t list -> ekikan_t list -> eki_t list  *)
let rec dijkstra_main lst lst_k = match lst with 
    [] -> []
    | _ -> let (p, undet) = saitan_wo_bunri2 lst in
        p::(dijkstra_main (koushin p undet lst_k) lst_k)

let%test _ = dijkstra_main
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
let dijkstra siten syuten = 
    let sorted = seiretsu global_ekimei_list in
    let siten_k = romaji_to_kanji siten sorted in 
    let syuten_k = romaji_to_kanji syuten sorted in 
    let init_eki = make_initiak_eki_list sorted siten_k in
    let result = dijkstra_main init_eki global_ekikan_list in 
    let rec find s lst = match lst with
        [] -> {namae=""; saitan_kyori=infinity; temae_list=[];}        
      | fst::rest -> if fst.namae = s then fst else find s rest
    in find syuten_k result

let%test _ = dijkstra "yoyogiuehara" "yoyogikouen" = {namae="代々木公園"; saitan_kyori=1.0; temae_list=["代々木公園";"代々木上原"];} 
let%test _ = dijkstra "omotesandou" "yoyogiuehara" = {namae="代々木上原"; saitan_kyori=3.1; temae_list=["代々木上原";"代々木公園";"明治神宮前";"表参道"];} 
let%test _ = dijkstra "myogadani" "korakuen" = {namae = "後楽園"; saitan_kyori = 1.8; temae_list = ["後楽園"; "茗荷谷"];}
