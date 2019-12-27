open Chap9
type eki_t = {
    namae        : string;      (* 駅名 *)
    saitan_kyori : float;       (* 最短距離 *)
    temae_list   : string list; (* 駅名リスト *)
}

(* 目的: ekimei_tのリストからeki_tのリストを作る *)
(* make_eki_list: ekimei_t list -> eki_t list *)
let rec make_eki_list lst = match lst with 
    [] -> []
  |  {kanji=k; kana=_; romaji=_; shozoku=_;}::rest -> {namae=k; saitan_kyori=infinity; temae_list=[];} :: make_eki_list rest

let%test _ = make_eki_list [
    {kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
    {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
    ]
    = [
    {namae="代々木上原"; saitan_kyori=infinity; temae_list=[];};
    {namae="代々木公園"; saitan_kyori=infinity; temae_list=[];};
    ]

(* 目的: eki_tのリストと起点から起点を初期化したリストを返す *)
(* shokika: eki_t list -> string -> eki_t list *)
let rec shokika lst name = match lst with 
    [] -> []
  | fst::rest -> let {namae=n; saitan_kyori=_; temae_list=_;} = fst in
                    if n = name 
                        then {namae=name; saitan_kyori=0.0; temae_list=[name;]}::shokika rest name
                        else fst::shokika rest name

let%test _ = shokika [
    {namae="代々木上原"; saitan_kyori=infinity; temae_list=[];};
    {namae="代々木公園"; saitan_kyori=infinity; temae_list=[];};
    ] "代々木公園"
    = [
    {namae="代々木上原"; saitan_kyori=infinity; temae_list=[];};
    {namae="代々木公園"; saitan_kyori=0.0; temae_list=["代々木公園";];};   
    ]

(* 目的: ekimei_tのリストをひらがなの順にソートし、駅名の重複を取り除いたekime_tのリストを返す *)
(* seiretsu: ekimei_t list -> ekimei_t list *)
let rec insert_e lst e = match lst with 
    [] -> [e]
  | fst::rest -> let {kanji=_; kana=k1; romaji=_; shozoku=_} = fst in
                 let {kanji=_; kana=k2; romaji=_; shozoku=_} = e in 
                 if k2 <= k1 then e :: fst :: rest 
                    else fst :: insert_e rest e

let rec ins_sort_e lst = match lst with 
    [] -> []
  | fst :: rest -> insert_e (ins_sort_e rest) fst

let%test _ = ins_sort_e [
    {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
    {kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"}; 
    {kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="銀座線"}; 
    {kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="半蔵門線"}; 
    ]
    = [
    {kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="銀座線"}; 
    {kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="半蔵門線"}; 
    {kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"}; 
    {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
    ] 

let rec seiretsu lst = let s_list = ins_sort_e lst in 
    match s_list with 
        [] -> []
        | fst::[] -> [fst] 
        | fst::snd::rest -> let {kanji=_; kana=k1; romaji=_; shozoku=_} = fst in
                            let {kanji=_; kana=k2; romaji=_; shozoku=_} = snd in
                            if k1 = k2 
                                then fst::seiretsu rest
                                else fst::snd::seiretsu rest   

let%test _ = seiretsu [
    {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
    {kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"}; 
    {kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="銀座線"}; 
    {kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="半蔵門線"}; 
    ]
    = [
    {kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="銀座線"}; 
    {kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"}; 
    {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
    ] 