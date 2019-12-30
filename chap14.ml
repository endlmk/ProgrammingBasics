open Chap8
open Chap9
open Chap10
open Chap12

(* 目的: 整数のリストから偶数の要素のみを含むリストを返す *)
(* even: int list -> int list *)
let even lst = let is_even x = x mod 2 = 0 in
  List.filter is_even lst

let%test _ = even [1;3;5;7] = []
let%test _ = even [2;8;4;3;5] = [2;8;4]
let%test _ = even [1;3;0;5;7] = [0] 

(* 目的: 学生リストから成績Aの人の数を返す *)
(* count_A: gakusei_t list -> int *)

let count_A lst = let is_seiseki_A g = g.seiseki = "A" in
  List.length (List.filter is_seiseki_A lst) 

let%test _ = count_A [
    {namae="asai";tensuu=70;seiseki="B";};
  ] = 0
let%test _ = count_A [
    {namae="asai";tensuu=70;seiseki="B";};
    {namae="asai";tensuu=80;seiseki="A";};
    {namae="asai";tensuu=90;seiseki="A";};
  ] = 2

(* 目的: 文字列を前から順にくっつけた文字列を返す *)
let concat lst = List.fold_right (^) lst "" 

let%test _ = concat ["春";"夏";"秋";"冬"] = "春夏秋冬"
let%test _ = concat ["1";"2";"3"] = "123"
let%test _ = concat ["春休み";"夏休み";"秋";"冬休み"] = "春休み夏休み秋冬休み"

(* 目的: 学生のリストから全員の得点の合計を返す *)
(* gakusei_sum: gakusei_t list -> int *)
let gakusei_sum lst = let add_tennsuu g a = g.tensuu + a in 
  List.fold_right add_tennsuu lst 0

let%test _ = gakusei_sum [
    {namae="asai";tensuu=70;seiseki="B";};
  ] = 70
let%test _ = gakusei_sum [
    {namae="asai";tensuu=70;seiseki="B";};
    {namae="asai";tensuu=80;seiseki="A";};
    {namae="asai";tensuu=90;seiseki="A";};
  ] = 240

let count lst s = let is_s s g = g.seiseki = s in
  List.length (List.filter (is_s s) lst) 

let sq_minus = fun n -> n ** 2.0 -. 1.0 

let get_name = fun p -> let {name=n;hight=_;weight=_;birth_month=_;birth_day=_;blood=_;} = p in n

(* 目的: ekimei_tのリストからeki_tのリストを作る *)
(* make_eki_list: ekimei_t list -> eki_t list *)
let make_eki_list2 lst = List.map (fun em -> let {kanji=k;kana=_;romaji=_;shozoku=_;} = em in {namae=k;saitan_kyori=infinity; temae_list=[];}) lst

let%test _ = make_eki_list2 [
    {kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
    {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
  ]
             = [
               {namae="代々木上原"; saitan_kyori=infinity; temae_list=[];};
               {namae="代々木公園"; saitan_kyori=infinity; temae_list=[];};
             ]


(* 目的: eki_tのリストと起点から起点を初期化したリストを返す *)
(* shokika: eki_t list -> string -> eki_t list *)
let shokika2 lst name = List.map 
    (fun e -> let {namae=n; saitan_kyori=_; temae_list=_;} = e in
      if n = name 
      then {namae=name; saitan_kyori=0.0; temae_list=[name;]}
      else e) lst

let%test _ = shokika2 [
    {namae="代々木上原"; saitan_kyori=infinity; temae_list=[];};
    {namae="代々木公園"; saitan_kyori=infinity; temae_list=[];};
  ] "代々木公園"
             = [
               {namae="代々木上原"; saitan_kyori=infinity; temae_list=[];};
               {namae="代々木公園"; saitan_kyori=0.0; temae_list=["代々木公園";];};   
             ]

(* 目的: make_eki_listとshoikaを同時に行う *)
(* make_initiak_eki_list: ekimei_t list -> eki_t list *)
let make_initiak_eki_list lst name = List.map (fun em -> let {kanji=k;kana=_;romaji=_;shozoku=_;} = em in
    if k = name
    then {namae=name; saitan_kyori=0.0; temae_list=[name;]}
    else {namae=k;saitan_kyori=infinity; temae_list=[];}) lst

let%test _ = make_initiak_eki_list [
    {kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
    {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
  ] "代々木公園"
             = [
               {namae="代々木上原"; saitan_kyori=infinity; temae_list=[];};
               {namae="代々木公園"; saitan_kyori=0.0; temae_list=["代々木公園";];};   
             ]
let rec enumerate n = 
    if n = 0 then [] else n :: enumerate (n-1)

(* 目的: 1から受け取った自然数までの和を返す *)
(* one_to_n: int -> int *)
let one_to_n n = List.fold_right (+) (enumerate n) 0

let%test _ = one_to_n 3 = 6
let%test _ = one_to_n 10 = 55

(* 目的: 階乗を求める *)
(* fac: int -> int *)
let fac n = List.fold_right ( * ) (enumerate n) 1

let%test _ = fac 5 = 120
let%test _ = fac 4 = 24
