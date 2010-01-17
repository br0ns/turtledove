

signature Heap =
sig
    eqtype key
    type 'a t

    val empty      : 'a t
    val isEmpty    : 'a t -> bool                            
    val insert : 'a t -> key * 'a -> 'a t

    (* Deletes the element the sorts as the smallest element given the ordering of the key*)
    val delete     : 'a t -> 'a t
    val min        : 'a t -> 'a
    val merge      : 'a t -> 'a t -> 'a t

    val insertList : 'a t -> (key * 'a) list -> 'a t
    val map        : ('a -> 'b) -> 'a t -> 'b t
    val size       : 'a t -> int

    (* aliases for insert, delete and min *)
    val push       : 'a t -> key * 'a -> 'a t
    val pop        : 'a t -> 'a t
    val peek       : 'a t -> 'a

    val peeki      : 'a t -> key * 'a
    val toList     : 'a t -> 'a list
    val toListi    : 'a t -> (key * 'a) list

    val toString   : (key -> string) -> ('a -> string) -> 'a t -> string
end
