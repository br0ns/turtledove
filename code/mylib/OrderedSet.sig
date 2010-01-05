(* Finite ordered set data structure *)

signature OrderedSet =
sig
    eqtype element
    type t

    val empty      : t

    val singleton  : element -> t

    (* Does nothing if the element is already in the set *)
    val insert     : t -> element -> t

    (* Does nothing if the element is not in the set *)
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
    val fold       : (element * 'a -> 'a) -> 'a -> t -> 'a
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

    (* A flat "visual string" that represents the OrderedSet *)
    val toString   : t -> string

    (* A "visual string" that represents the OrderedSet in a fancy way  *)
    val show       : t -> Report.t
end
