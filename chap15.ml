open Chap12

(* 目的：ユークリッドの互除法により最大公約数を求める *)
(* gcd: int -> int -> int *)
let rec  gcd m n = 
    if n = 0 then m
    else gcd n (m mod n)

let%test _ = gcd 60 40 = 20
let%test _ = gcd 72 50 = 2
let%test _ = gcd 90 36 = 18

(* 目的: エラトステネスのふるいにより素数を求める *)
(* sieve: int list -> int list *)
let rec sieve lst = match lst with
    [] -> []
  | fst::rest -> let sieved = List.filter (fun x -> (x mod fst) != 0) rest in
                    fst::(sieve sieved) 

(* prime: int -> int list *)
let prime n = sieve (List.init (n-1) (fun x -> x + 2))

let%test _ = prime 9 = [2;3;5;7]
let%test _ = prime 15 = [2;3;5;7;11;13]
let%test _ = prime 23 = [2;3;5;7;11;13;17;19;23]

(* 目的: eki_tのリストから最短距離最小の駅とそれ以外の駅からなるリストの組を返す *)
(* saitan_wo_bunri: eki_t list -> eki_t * eki_t list *)
let rec find_min lst = match lst with
    [] -> {namae=""; saitan_kyori=infinity; temae_list=[];}
  | fst::rest -> let {namae=_; saitan_kyori=s; temae_list=_;} = fst in
                 let min_t = find_min rest in
                 let {namae=_; saitan_kyori=s_t; temae_list=_;} = min_t in
                 if s <= s_t then fst else min_t

let saitan_wo_bunri lst = match lst with
    [] -> ({namae=""; saitan_kyori=infinity; temae_list=[];},[])
  | _ -> let e = find_min lst in (e, List.filter (fun x -> let {namae=n; saitan_kyori=_; temae_list=_;} = x in n != e.namae) lst)


let%test _ = saitan_wo_bunri
    [ 
        {namae="代々木公園"; saitan_kyori=1.0; temae_list=["代々木上原"];}; 
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

let saitan_wo_bunri2 lst = 
    let update_min = fun e ((p_e:eki_t), (p_l:eki_t list)) -> 
        let {namae=_; saitan_kyori=s_e; temae_list=_;} = e in 
        let {namae=_; saitan_kyori=s_p; temae_list=_;} = p_e in
        if p_e.namae = "" then (e, [])
        else if s_e <= s_p 
            then (e, p_e::p_l)
            else (p_e, e::p_l) in
    List.fold_right update_min lst ({namae=""; saitan_kyori=infinity; temae_list=[];},[])

let%test _ = saitan_wo_bunri2
    [ 
        {namae="代々木公園"; saitan_kyori=1.0; temae_list=["代々木上原"];}; 
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