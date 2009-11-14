

signature Tree =
sig
    type 'a tree
    eqtype node

    val root : node

    val create : 'a -> 'a tree
    val insert : 'a tree -> node -> 'a -> (node * 'a tree)
    val insertList : 'a tree -> node -> 'a list -> (node list * 'a tree)
    val insertTree : 'a tree -> node -> 'a tree -> (node * 'a tree)

    val delete : 'a tree -> node -> 'a tree

    val value : 'a tree -> node -> 'a
    val children : 'a tree -> node -> node list
    val parent : 'a tree -> node -> node option
    val change : 'a tree -> node -> ('a -> 'a) -> 'a tree
    val update : 'a tree -> node -> 'a -> 'a tree

    val size : 'a tree -> int
    val height : 'a tree -> int

    val map : ('a -> 'b) -> 'a tree -> 'b tree
    val foldpr : ('a * 'b -> 'b) -> 'b -> 'a tree -> 'b
    val foldin : ('a * 'b -> 'b) -> 'b -> 'a tree -> 'b
    val foldpo : ('a * 'b -> 'b) -> 'b -> 'a tree -> 'b
end
