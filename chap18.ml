open Chap8
open Chap9
open Chap12
open Chap17

(* 目的: person_tのリストを受け取って最初のA型の人をオプション型で返す *)
(* first_A: person_t list -> person_t option *)
let rec first_A lst = match lst with
    [] -> None
  | fst::rest -> if fst.blood = "A" then Some fst else first_A rest

let list1 = [
    {name="aaa";hight=123.4;weight=60.0;birth_month=6;birth_day=26;blood="A"};
    {name="aaa";hight=123.4;weight=60.0;birth_month=6;birth_day=26;blood="B"};
    {name="aaa";hight=123.4;weight=60.0;birth_month=6;birth_day=26;blood="AB"};
]
let list2 = [
    {name="aaa";hight=123.4;weight=60.0;birth_month=6;birth_day=26;blood="B"};
    {name="aaa";hight=123.4;weight=60.0;birth_month=6;birth_day=26;blood="AB"};
]

let%test _ = first_A list1 = Some {name="aaa";hight=123.4;weight=60.0;birth_month=6;birth_day=26;blood="A"}
let%test _ = first_A list2 = None

(* 目的: 野菜のリストと八百屋のリストから八百屋においていない野菜の数を返す *)
(* count_urikire_yasai: string list -> (string * int) list -> int *)
let rec count_urikire_yasai yasai_list yaoya_list = 
    let rec price item yaoya_list = match yaoya_list with
        [] -> None
      | (yasai, nedan)::rest -> if yasai = item then Some nedan else price item rest
    in 
    match yasai_list with
        [] -> 0
      | fst::rest -> match price fst yaoya_list with
            Some _ -> count_urikire_yasai rest yaoya_list
          | None -> 1 + count_urikire_yasai rest yaoya_list


let yaoya_list = [
    ("トマト",200);
    ("たまねぎ",100);
    ("にんじん",100);
]

let%test _ = count_urikire_yasai ["じゃがいも"; "ごぼう"; "にんじん"] yaoya_list = 2
let%test _ = count_urikire_yasai ["トマト"; "ごぼう"; "たまねぎ"] yaoya_list = 1

(* 目的: 駅名と駅名の距離の組のリストからその駅までの距離を返す *)
(* assoc: string -> (string * float) list -> float *)
let rec assoc s lst = match lst with 
    [] -> raise Not_found
  | fst::rest -> let (s_l, l_l) = fst in
                 if s_l = s then l_l else assoc s rest

let%test _ = assoc "後楽園" [("新大塚", 1.2); ("後楽園", 1.8)] = 1.8
let%test _ = (try assoc "池袋" [("新大塚", 1.2); ("後楽園", 1.8)] with Not_found -> infinity) = infinity

(* 目的:駅名2つとekikan_tree_tから2つの駅間距離を返す *)
(* get_ekikan_kyori: string -> string -> ekikan_tree_t -> float *)
let rec get_ekikan_kyori2 e_f e_t t = match t with
    Empty -> raise Not_found
  | Node(t1, (e_n, e_l), t2) -> if e_n = e_f then assoc e_t e_l
                                else if e_f < e_n then get_ekikan_kyori2 e_f e_t t1
                                else get_ekikan_kyori2 e_f e_t t2

let%test _ = get_ekikan_kyori2 "代々木公園" "代々木上原" ekikan_tree2 = 1.0
let%test _ = get_ekikan_kyori2 "代々木公園" "明治神宮前" ekikan_tree2 = 1.2
let%test _ = (try get_ekikan_kyori2 "代々木上原" "明治神宮前" ekikan_tree2 with Not_found -> infinity) = infinity

(* 目的: 直前に確定した駅pと未確定の駅のリストv,駅間距離リストkから更新処理を行ったリストを返す *)
(* koushin: eki_t -> eki_t list -> ekikan_t list -> eki_t list *)
let koushin3 p lst lst_k = 
    let ekikan_tree = inserts_ekikan Empty lst_k in 
    let update_eki p v = 
        try let k = get_ekikan_kyori p.namae v.namae ekikan_tree in
            if (p.saitan_kyori +. k) < v.saitan_kyori 
                then {namae=v.namae;saitan_kyori=(p.saitan_kyori +. k);temae_list=v.namae::p.temae_list}
                else v
        with Not_found -> v
    in List.map (fun e -> update_eki p e) lst  

let%test _ = koushin3
    {namae="代々木上原"; saitan_kyori=0.0; temae_list=["代々木上原"];}
    [ {namae="代々木公園"; saitan_kyori=infinity; temae_list=[];}; 
      {namae="明治神宮前"; saitan_kyori=infinity; temae_list=[];}; ]
      global_ekikan_list = 
    [ {namae="代々木公園"; saitan_kyori=1.0; temae_list=["代々木公園"; "代々木上原"];}; 
      {namae="明治神宮前"; saitan_kyori=infinity; temae_list=[];}; ]

exception No_such_station of string

(* 目的: 駅名リストからその駅の漢字表記を返す *)
(* romaji_to_kanji: string -> ekimei_t list -> string *)
let rec romaji_to_kanji2 romaji lst = match lst with 
    [] -> raise (No_such_station (romaji))
  | {kanji=k;kana=_;romaji=r;shozoku=_}::rest -> if r = romaji 
                                                    then k 
                                                    else romaji_to_kanji2 romaji rest

let%test _ = romaji_to_kanji2 "myogadani" global_ekimei_list = "茗荷谷"
let%test _ = romaji_to_kanji2 "shibuya" global_ekimei_list = "渋谷"
let%test _ = (try romaji_to_kanji2 "kameoka" global_ekimei_list with No_such_station st -> st)= "kameoka"