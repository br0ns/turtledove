

signature Heap =
sig
    eqtype key
    type 'a heap

    val empty      : 'a heap
    val insert     : 'a heap -> key * 'a -> 'a heap
    val push       : 'a heap -> key * 'a -> 'a heap
    val insertList : 'a heap -> (key * 'a) list -> 'a heap
    val merge      : 'a heap -> 'a heap -> 'a heap
    val map        : ('a -> 'b) -> 'a heap -> 'b heap
    val isEmpty    : 'a heap -> bool
    val size       : 'a heap -> int
    val pop        : 'a heap -> 'a
    val popi       : 'a heap -> key * 'a
    val peek       : 'a heap -> 'a
    val peeki      : 'a heap -> key * 'a
    val toList     : 'a heap -> 'a list
    val toListi    : 'a heap -> (key * 'a) list

    val toString   : (key -> string) -> ('a -> string) -> 'a heap -> string
end
