
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