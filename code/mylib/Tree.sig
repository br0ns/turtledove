

signature Tree =
sig
    type 'a t
    eqtype node

    val root : node

    val create : 'a -> 'a t
    val insert : 'a t -> node -> 'a -> (node * 'a t)
    val insertList : 'a t -> node -> 'a list -> (node list * 'a t)
    val insertTree : 'a t -> node -> 'a t -> (node * 'a t)

    val delete : 'a t -> node -> 'a t

    val value : 'a t -> node -> 'a
    val children : 'a t -> node -> node list
    val parent : 'a t -> node -> node option
    val change : 'a t -> node -> ('a -> 'a) -> 'a t
    val update : 'a t -> node -> 'a -> 'a t

    val size : 'a t -> int
    val height : 'a t -> int

    val map : ('a -> 'b) -> 'a t -> 'b t
    val foldpr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
    val foldin : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
    val foldpo : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
end
