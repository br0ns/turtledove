(* Finite ordered set data structure *)

signature OrderedSet =
sig
    eqtype element
    eqtype t

    val empty      : t

    val singleton  : element -> t
    val insert     : t -> element -> t
    val delete     : t -> element -> t
    val fromList   : element list -> t

    val union      : t -> t -> t
    val inter      : t -> t -> t
    val diff       : t -> t -> t

    val subset     : t -> t -> bool
    val equal      : t -> t -> bool
    val member     : t -> element -> bool
    val isEmpty    : t -> bool

    val compare    : t -> t -> order

    (* Returns an ordered list of elements of the set *)
    val toList     : t -> element list

    val card       : t -> int

    val partition  : (element -> bool) -> t -> t * t
    val filter     : (element -> bool) -> t -> t
    val exists     : (element -> bool) -> t -> bool
    val all        : (element -> bool) -> t -> bool
    val find       : (element -> bool) -> t -> element option

    val app        : (element -> unit) -> t -> unit
    val map        : (element -> element) -> t -> t
    val mapPartial : (element -> element option) -> t -> t
    val foldl      : (element * 'a -> 'a) -> 'a -> t -> 'a
    val foldr      : (element * 'a -> 'a) -> 'a -> t -> 'a

    val split      : t -> element * t
    val splitLeast : t -> element * t
    val splitGreatest : t -> element * t

    val least      : t -> element
    (* Least element in the set. May raise Empty *)

    val greatest   : t -> element
    (* Greatest element in the set. May raise Empty *)

    (* Some element in the set. May raise Empty *)
    val some       : t -> element

    (* Takes a printing function for element and a triple of left, right and delimitor *)
    val toString   : (element -> string) -> string * string * string-> t -> string
end
