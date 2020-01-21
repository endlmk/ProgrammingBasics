type 'a t
val empty : 'a t
val singleton : 'a -> 'a t
val union : 'a t -> 'a t -> 'a t
val inter : 'a t -> 'a t -> 'a t
val diff  : 'a t -> 'a t -> 'a t
val mem : 'a -> 'a t -> bool 