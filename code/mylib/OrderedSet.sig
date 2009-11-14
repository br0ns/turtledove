(* Finite ordered set data structure *)

signature OrderedSet =
sig
    eqtype element
    eqtype set

    val empty      : set

    val singleton  : element -> set
    val insert     : set -> element -> set
    val delete     : set -> element -> set
    val fromList   : element list -> set
    val insertList : set -> element list -> set

    val union      : set -> set -> set
    val inter      : set -> set -> set
    val diff       : set -> set -> set

    val subset     : set -> set -> bool
    val equal      : set -> set -> bool
    val member     : set -> element -> bool
    val isEmpty    : set -> bool

    val compare    : set -> set -> order

    (* Returns a ordered list of elements of the set *)
    val toList     : set -> element list

    val card       : set -> int

    val partition  : (element -> bool) -> set -> set * set
    val filter     : (element -> bool) -> set -> set
    val remove     : (element -> bool) -> set -> set
    val exists     : (element -> bool) -> set -> bool
    val all        : (element -> bool) -> set -> bool
    val find       : (element -> bool) -> set -> element option

    val app        : (element -> unit) -> set -> unit
    val map        : (element -> element) -> set -> set
    val mapPartial : (element -> element option) -> set -> set
    val foldl      : (element * 'a -> 'a) -> 'a -> set -> 'a
    val foldr      : (element * 'a -> 'a) -> 'a -> set -> 'a

    val split      : set -> (element * set) option

    val least      : set -> element
    (* Least element in the set. May raise Empty *)

    val greatest   : set -> element
    (* Greatest element in the set. May raise Empty *)

    (* Some element in the set. May raise Empty *)
    val some       : set -> element

    (* Takes a printing function for ''a and a triple of left, right and delimitor *)
    val toString   : (element -> string) -> string * string * string-> set -> string
end
