(* 2分探索木を表すモジュールのシグネチャ *)
type ('a, 'b) t
(* キーが'a, 値が'bの木の型 *)

val empty : ('a, 'b) t
(* empty *)
(* 空の木 *)

val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t 
(* insert tree key value *)
(* 木treeにキーと値を挿入した木を返す *)
(* キーが存在した場合新しい値に置き換える *)

val search : ('a, 'b) t -> 'a -> 'b
(* search tree key *)
(* 木からキーに対応する値を返す *)
(* キーに対応する値がない場合Not_foundをraiseする *)