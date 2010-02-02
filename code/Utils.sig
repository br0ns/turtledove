(* Utility functions *)

signature Utils =
sig
    val ofSome        : 'a option -> 'a
    val ofSomeWithMsg : string -> 'a option -> 'a

    val id : 'a -> 'a

    val ^* : ('a -> 'a) * int -> 'a -> 'a

    val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
    val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

    val to : int * int -> int list

    val inc : int ref -> int
    val dec : int ref -> int

    val leftmost  : 'a option list -> 'a option
    val rightmost : 'a option list -> 'a option
end
