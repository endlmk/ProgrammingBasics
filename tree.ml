
module Tree : sig
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
end = struct
    type ('a, 'b) t = Empty
                    | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t

    let empty = Empty

    let rec insert tree k v = match tree with
        Empty -> Node (Empty, k, v, Empty)
    | Node(left, key, value, right) ->
            if k = key then Node(left, key, v, right)
            else if k < key 
                then Node(insert left k v, key, value, right)
                else Node(left, key, value, insert right k v)

    let rec search tree k = match tree with
        Empty -> raise Not_found
    | Node(left, key, value, right) ->
            if k = key then value
            else if k < key 
                then search left k
                else search right k
end