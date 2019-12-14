open Chap8
open Chap9
(* 昇順に並んだ整数リストと整数から整列したリストを返す *)
(* insert: int list -> int -> int list *)
let rec insert lst n = match lst with 
    [] -> [n]
  | fst :: rest -> if n < fst then n :: fst :: rest 
                              else fst :: insert rest n

let%test _ = insert [1;3;4;7;8] 5 = [1;3;4;5;7;8]
let%test _ = insert [1;3;5;6] 9 = [1;3;5;6;9]
let%test _ = insert [2;3;4;] 1 = [1;2;3;4]

(* 目的: 挿入ソートを行う *)
(* ins_sort: int list -> int list *)
let rec ins_sort lst = match lst with 
    [] -> []
  | fst :: rest -> insert (ins_sort rest) fst

let%test _ = ins_sort [5;3;8;1;7;4] = [1;3;4;5;7;8]
let%test _ = ins_sort [6;3;9;1] = [1;3;6;9]
let%test _ = ins_sort [1;2;3;5;9] = [1;2;3;5;9]

type gakusei_t = {
    namae : string;   (* 名前 *)
    tensuu : int;     (* 点数 *)
    seiseki : string; (* 成績 *)
}

(* 目的: 学生のリストを成績でソートする *)
(* gakusei_sort: gakusei_t list -> gakusei_t list *)
let rec gakusei_ins lst g = match lst with
    [] -> [g]
  | fst ::rest -> match fst with {namae=_; tensuu=t1; seiseki=_; } 
                    -> match g with  {namae=_; tensuu=t2; seiseki=_; }
                        -> if t2 < t1 then g :: fst ::rest 
                                     else fst :: gakusei_ins rest g

let rec gakusei_sort lst = match lst with
    [] -> []
  | fst :: rest -> gakusei_ins (gakusei_sort rest) fst

let%test _ = gakusei_sort [{namae="a";tensuu=30;seiseki="C"};
                           {namae="a";tensuu=10;seiseki="E"};
                           {namae="a";tensuu=80;seiseki="A"};]
                        = [{namae="a";tensuu=10;seiseki="E"};
                           {namae="a";tensuu=30;seiseki="C"};
                           {namae="a";tensuu=80;seiseki="A"};]

(* 目的: person_tを名前の順に整列したリストを返す *)
(* person_sort: person_t list -> person_t list *)
let person_sort lst = List.sort (fun pl pr -> String.compare pl.name pr.name) lst

let%test _ = person_sort [{ name = "bear"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "A"; };
                          { name = "order"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "AB"; };
                          { name = "neko"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "O"; };]
                       = [{ name = "bear"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "A"; };
                          { name = "neko"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "O"; };
                          { name = "order"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "AB"; };]

(* 目的: gakusei_tのリストから最高得点を取った人のレコードを返す *)
(* gakusei_max: gakusei_t list -> gakusei_t *)
let rec gakusei_max lst = match lst with
    [] -> {namae="";tensuu=0;seiseki=""}
  | fst :: rest -> match fst with {namae=_;tensuu=t;seiseki=_}
                    -> match gakusei_max rest with {namae=_;tensuu=tm;seiseki=_} 
                        -> if t > tm then fst else gakusei_max rest

let%test _ = gakusei_max [{namae="a";tensuu=30;seiseki="C"};
                           {namae="a";tensuu=10;seiseki="E"};
                           {namae="a";tensuu=80;seiseki="A"};]
                        =  {namae="a";tensuu=80;seiseki="A"}
let%test _ = gakusei_max [{namae="a";tensuu=55;seiseki="C"};
                           {namae="a";tensuu=35;seiseki="E"};
                           {namae="a";tensuu=40;seiseki="A"};]
                        =  {namae="a";tensuu=55;seiseki="C"}

(* 目的: gakusei_tのリストから最高得点を取った人のレコードを返す *)
(* gakusei_max2: gakusei_t list -> gakusei_t *)
let rec gakusei_max2 lst = match lst with
    [] -> {namae="";tensuu=0;seiseki=""}
  | fst :: rest -> match fst with {namae=_;tensuu=t;seiseki=_} 
                    -> let gakusei_m = gakusei_max2 rest in 
                        match gakusei_m with {namae=_;tensuu=tm;seiseki=_} 
                        -> if t > tm then fst else gakusei_m

let%test _ = gakusei_max2 [{namae="a";tensuu=30;seiseki="C"};
                           {namae="a";tensuu=10;seiseki="E"};
                           {namae="a";tensuu=80;seiseki="A"};]
                        =  {namae="a";tensuu=80;seiseki="A"}
let%test _ = gakusei_max2 [{namae="a";tensuu=55;seiseki="C"};
                           {namae="a";tensuu=35;seiseki="E"};
                           {namae="a";tensuu=40;seiseki="A"};]
                        =  {namae="a";tensuu=55;seiseki="C"}

(* 目的: person_tのリストから各血液型の人数を集計した組を返す *)
(* ketsueki_shukei: person_t list -> int * int * int * int *)
let rec ketsueki_shukei lst = match lst with
    [] -> (0,0,0,0)
  | {name=_;hight=_;weight=_;birth_month=_;birth_day=_;blood=bl;} :: rest ->
      let (a,b,o,ab) = ketsueki_shukei rest in 
        if bl = "A" then (a+1,b,o,ab) 
        else if bl = "B" then (a,b+1,o,ab)
        else if bl = "O" then (a,b,o+1,ab)
        else (a,b,o,ab+1)

let%test _ = ketsueki_shukei [{ name = "bear"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "A"; };
                              { name = "neko"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "O"; };
                              { name = "order"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "AB"; }]
            = (1,0,1,1)

(* 目的: person_tのリストから最多の血液型を返す *)
(* saita_ketsueki: person_t list -> string *)
let saita_ketsueki lst = let (a,b,o,ab) = ketsueki_shukei lst in
    if a > b && a > o  && a > ab then "A"
    else if b > a && b > o && b > ab then "B"
    else if o > a && o > b && o > ab then "O"
    else if ab > a && ab > b && ab > o then "AB"
    else ""

let%test _ = saita_ketsueki [{ name = "bear"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "A"; };
                              { name = "neko"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "O"; };
                              { name = "order"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "AB"; }]
            = ""
let%test _ = saita_ketsueki [{ name = "bear"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "A"; };
                              { name = "neko"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "O"; };
                              { name = "order"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "AB"; };
                               { name = "order"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "AB"; }]
            = "AB"

(* 目的: ２つのリストの長さが同じかどうかを返す *)
(* equal_length: a' list -> a' list -> bool *)
let rec equal_length lst1 lst2 = match (lst1, lst2) with
    ([],[]) -> true
  | (_,[]) -> false
  | ([],_) -> false
  | (_::rest1,_::rest2) -> equal_length rest1 rest2

let%test _ = equal_length [1;2;3] [4;5;6] = true
let%test _ = equal_length [1;2;3] [4;5] = false
let%test _ = equal_length [] [4;5] = false
let%test _ = equal_length ["a"; "b"] [4;5] = true

(* 目的: 駅名リストからその駅の漢字表記を返す *)
(* romaji_to_kanji: string -> ekimei_t list -> string *)
let rec romaji_to_kanji romaji lst = match lst with 
    [] -> ""
  | {kanji=k;kana=_;romaji=r;shozoku=_}::rest -> if r = romaji 
                                                    then k 
                                                    else romaji_to_kanji romaji rest

let%test _ = romaji_to_kanji "myogadani" global_ekimei_list = "茗荷谷"
let%test _ = romaji_to_kanji "shibuya" global_ekimei_list = "渋谷"
let%test _ = romaji_to_kanji "kameoka" global_ekimei_list = ""

(* 目的: 漢字の駅名２つからその2駅間の距離を返す *)
(* get_ekikan_kyori: string -> string -> float *)
let rec get_ekikan_kyori e1 e2 lst = match lst with
    [] -> infinity
  | {kiten=k;shuten=s;keiyu=_;kyori=ky;jikan=_;}::rest -> if (e1=k && e2=s) || (e1=s && e2=k)
                                                              then ky
                                                              else get_ekikan_kyori e1 e2 rest

let%test _ = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_list = 1.2
let%test _ = get_ekikan_kyori "三越前" "大手町" global_ekikan_list = 0.7
let%test _ = get_ekikan_kyori "九段下" "大手町" global_ekikan_list = infinity

(* 目的: ローマ字の駅名2つからその間の距離を調べ、文字列を返す *)
(* kyori_wo_hyoji: string -> string -> string *)
let kyori_wo_hyoji e1 e2 = let k1 = romaji_to_kanji e1 global_ekimei_list in
                              if k1 = "" then e1 ^ "という駅は存在しません" else
                                let k2 = romaji_to_kanji e2 global_ekimei_list in
                                  if k2 = "" then e2 ^ "という駅は存在しません" else
                                    let ky = get_ekikan_kyori k1 k2 global_ekikan_list in
                                      if ky = infinity then k1 ^ "と" ^ k2 ^ "はつながっていません" else
                                        k1 ^ "から" ^ k2 ^ "までは" ^ (string_of_float ky) ^ "kmです"


let%test _ = kyori_wo_hyoji "myogadani" "shinotsuka" = "茗荷谷から新大塚までは1.2kmです"
let%test _ = kyori_wo_hyoji "korakuen" "myogadani" = "後楽園から茗荷谷までは1.8kmです"
let%test _ = kyori_wo_hyoji "kudanshita" "otemachi" = "九段下と大手町はつながっていません"
let%test _ = kyori_wo_hyoji "korakuen" "kameoka" = "kameokaという駅は存在しません"