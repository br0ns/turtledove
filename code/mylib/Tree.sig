

signature Tree =
sig
    type 'a t
    eqtype node

    val root : node

    val create : 'a -> 'a t
    val insert : 'a t -> node -> 'a -> (node * 'a t)
    val insertTree : 'a t -> node -> 'a t -> (node * 'a t)

    val delete : 'a t -> node -> 'a t

    val value : 'a t -> node -> 'a
    val children : 'a t -> node -> node list
    val parent : 'a t -> node -> node option
    val sub : 'a t -> node -> 'a t
    val modify : ('a -> 'a) -> 'a t -> node -> 'a t
    val update : 'a t -> node -> 'a -> 'a t

    val toList : 'a t -> 'a list

    val size : 'a t -> int
    val height : 'a t -> int

    val map : ('a -> 'b) -> 'a t -> 'b t
    val foldpr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
    val foldin : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
    val foldpo : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
end
