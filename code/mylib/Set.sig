(* Finite set data structure *)

signature Set =
sig
    eqtype ''a set

    val empty      : ''a set
    val singleton  : ''a -> ''a set
    val insert     : ''a set -> ''a -> ''a set
    val delete     : ''a set -> ''a -> ''a set
    val fromList   : ''a list -> ''a set
    val insertList : ''a set -> ''a list -> ''a set

    val union      : ''a set -> ''a set -> ''a set
    val inter      : ''a set -> ''a set -> ''a set
    val diff       : ''a set -> ''a set -> ''a set

    val subset     : ''a set -> ''a set -> bool
    val equal      : ''a set -> ''a set -> bool
    val member     : ''a set -> ''a -> bool
    val isEmpty    : ''a set -> bool

    val toList     : ''a set -> ''a list

    val card       : ''a set -> int

    val collate    : (''a -> ''a -> order) -> ''a set -> ''a set -> order

    val partition  : (''a -> bool) -> ''a set -> ''a set * ''a set
    val filter     : (''a -> bool) -> ''a set -> ''a set
    val remove     : (''a -> bool) -> ''a set -> ''a set
    val exists     : (''a -> bool) -> ''a set -> bool
    val all        : (''a -> bool) -> ''a set -> bool
    val find       : (''a -> bool) -> ''a set -> ''a option

    val app        : (''a -> unit) -> ''a set -> unit
    val map        : (''a -> ''b) -> ''a set -> ''b set
    val mapPartial : (''a -> ''b option) -> ''a set -> ''b set
    val fold       : (''a * 'b -> 'b) -> 'b -> ''a set -> 'b

    val split      : ''a set -> (''a * ''a set) option

    (* May raise Empty *)
    val some       : ''a set -> ''a

    (* Takes a printing function for ''a and a triple of left, right and delimitor *)
    val toString   : (''a -> string) -> string * string * string-> ''a set -> string
end
