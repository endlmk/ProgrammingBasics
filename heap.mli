type ('a, 'b) t
(* 'aは最小値を求める型、'bは付加情報 *)

type index_t
(* ヒープの添字の型 *)

val create : int -> 'a -> 'b -> ('a, 'b) t 
(* ヒープのサイズと'aと'bのダミーの値を受け取り、空のヒープを返す *)

val insert : ('a, 'b) t -> 'a -> 'b -> index_t * ('a, 'b) t
(* ヒープに新しい要素を追加する。破壊的に書き換える *)

val get : ('a, 'b) t -> index_t -> 'a * 'b
(* ヒープのindex番目の要素を返す *)

val set : ('a, 'b) t -> index_t -> 'a -> 'b -> ('a, 'b) t
(* ヒープのindex番目の値を更新したヒープを返す。破壊的に書き換える。 *)

val split_top : ('a, 'b) t -> ('a *'b) * ('a, 'b) t
(* 最小の値を持つものとそれを除いたヒープの組を返す。破壊的に書き換える。 *)
