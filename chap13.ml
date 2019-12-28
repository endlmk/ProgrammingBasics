open Chap8
open Chap9
open Chap10
open Chap12

(* 目的: person_tのリストから指定された血液型の人の数を返す *)
(* count_ketsueki: person_t list -> string -> person_t list *)
let rec count_ketsueki lst k = match lst with
    [] -> 0
  | fst::rest -> if fst.blood = k 
                    then 1 + count_ketsueki rest k
                    else count_ketsueki rest k

let%test _ = count_ketsueki [
    { name = "bear"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "A"; };
    { name = "neko"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "O"; };
    { name = "order"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "AB"; };
    { name = "ord"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "A"; };
    ] "A" = 2

(* 目的: person_tのリストから出てくる人の名前のリストを返す *)
(* person_namae: person_t list -> string list *)
let get_namae p = p.name
let person_namae lst = List.map get_namae lst

let%test _ = person_namae [
    { name = "bear"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "A"; };
    { name = "neko"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "O"; };
    { name = "order"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "AB"; };
    { name = "ord"; hight = 157.; weight = 65.; birth_month = 1; birth_day = 31; blood = "A"; };
    ] = ["bear";"neko";"order";"ord"]
(* 目的: ふたつの関数を合成した関数を返す *)
(* compose: ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c *)
let compose f g = let h x = g (f x) in h

let twice f = 
    let g x = f (f x) in g

(* fがないと想定した型にならない！ *)
let twice2 f = twice twice f
let add3 x = x + 3
let test = twice2 add3

(* 目的: 直前に確定した駅pと未確定の駅qからpとqのつながりを調べ、qを更新する。 *)
(* koushin1: eki_t -> eki_t -> eki_t  *)
let koushin1 p q = let {namae=n1; saitan_kyori=_; temae_list=_;} = p in 
                   let {namae=n2; saitan_kyori=s; temae_list=t;} = q in 
                   let k = get_ekikan_kyori n1 n2 global_ekikan_list in
                        if k < s
                            then {namae=n2; saitan_kyori=k; temae_list=List.append t [n1]}
                            else q

let%test _ = koushin1 
    {namae="代々木上原"; saitan_kyori=infinity; temae_list=[];}
    {namae="代々木公園"; saitan_kyori=infinity; temae_list=[];} = 
    {namae="代々木公園"; saitan_kyori=1.0; temae_list=["代々木上原"];} 

let%test _ = koushin1 
    {namae="代々木上原"; saitan_kyori=infinity; temae_list=[];}
    {namae="明治神宮前"; saitan_kyori=infinity; temae_list=[];} = 
    {namae="明治神宮前"; saitan_kyori=infinity; temae_list=[];}
    
(* 目的: 直前に確定した駅pと未確定の駅のリストvから更新処理を行ったリストを返す *)
(* koushin: eki_t -> eki_t list -> eki_t list *)
let koushin p lst = List.map (koushin1 p) lst

let%test _ = koushin
    {namae="代々木上原"; saitan_kyori=infinity; temae_list=[];}
    [ {namae="代々木公園"; saitan_kyori=infinity; temae_list=[];}; 
      {namae="明治神宮前"; saitan_kyori=infinity; temae_list=[];}; ] = 
    [ {namae="代々木公園"; saitan_kyori=1.0; temae_list=["代々木上原"];}; 
      {namae="明治神宮前"; saitan_kyori=infinity; temae_list=[];}; ]
