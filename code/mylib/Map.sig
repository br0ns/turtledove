(* Finite map data structure *)

signature Map =
sig
    type (''a, 'b) map

    val empty       : (''a, 'b) map

    val singleton   : ''a * 'b -> (''a, 'b) map

    (* Insert if key is not in map *)
    val insert      : (''a, 'b) map -> ''a * 'b -> (''a, 'b) map option
    val fromList    : (''a * 'b) list -> (''a, 'b) map

    val delete      : (''a, 'b) map -> ''a -> (''a, 'b) map
    val update      : (''a, 'b) map -> (''a * 'b) -> (''a, 'b) map
    val updateList  : (''a, 'b) map -> (''a * 'b) list -> (''a, 'b) map
    val lookup      : (''a, 'b) map -> ''a -> 'b option

    val inDomain    : (''a, 'b) map -> ''a -> bool
    val isEmpty     : (''a, 'b) map -> bool

    val size        : (''a, 'b) map -> int

    val toList      : (''a, 'b) map -> (''a * 'b) list
    val domain      : (''a, 'b) map -> ''a list
    val range       : (''a, 'b) map -> 'b list
    val split       : (''a, 'b) map -> ((''a * 'b) * (''a, 'b) map) option

    val collate     : ('b -> 'b -> order) -> (''a, 'b) map -> (''a, 'b) map -> order

    val partition   : ('b -> bool) -> (''a, 'b) map -> (''a, 'b) map * (''a, 'b) map
    val partitioni  : (''a * 'b -> bool) -> (''a, 'b) map -> (''a, 'b) map * (''a, 'b) map
    val filter      : ('b -> bool) -> (''a, 'b) map -> (''a, 'b) map
    val filteri     : (''a * 'b -> bool) -> (''a, 'b) map -> (''a, 'b) map
    val remove      : ('b -> bool) -> (''a, 'b) map -> (''a, 'b) map
    val removei     : (''a * 'b -> bool) -> (''a, 'b) map -> (''a, 'b) map
    val exists      : ('b -> bool) -> (''a, 'b) map -> bool
    val existsi     : (''a * 'b -> bool) -> (''a, 'b) map -> bool
    val all         : ('b -> bool) -> (''a, 'b) map -> bool
    val alli        : (''a * 'b -> bool) -> (''a, 'b) map -> bool
    val find        : ('b -> bool) -> (''a, 'b) map -> ''a * 'b
    val findi       : (''a * 'b -> bool) -> (''a, 'b) map -> ''a * 'b

    val app         : ('b -> unit) -> (''a, 'b) map -> unit
    val appi        : (''a * 'b -> unit) -> (''a, 'b) map -> unit
    val map         : ('b -> 'c) -> (''a, 'b) map -> (''a, 'c) map
    val mapi        : (''a * 'b -> 'c) -> (''a, 'b) map -> (''a, 'c) map
    val fold        : ('b * 'c -> 'c) -> 'c -> (''a, 'b) map -> 'c
    val foldi       : ((''a * 'b) * 'c -> 'c) -> 'c -> (''a, 'b) map -> 'c

    val union       : ('b -> 'b -> 'b) -> (''a, 'b) map -> (''a, 'b) map -> (''a, 'b) map
    val unioni      : ('a -> 'b -> 'b -> 'b) -> (''a, 'b) map -> (''a, 'b) map -> (''a, 'b) map 
    (* return a map whose domain is the union of the domains of the two input
     * maps, using the supplied function to define the map on elements that
     * are in both domains.
     *)

    (* return a map whose domain is the intersection of the domains of the
     * two input maps, using the supplied function to define the range.
     *)
    val inter       : ('b -> 'c -> 'd) -> (''a, 'b) map -> (''a, 'c) map -> (''a, 'd) map
    val interi      : (''a -> 'b -> 'c -> 'd) -> (''a, 'b) map -> (''a, 'c) map -> (''a, 'd) map

    (* merge two maps using the given function to control the merge. For
     * each key k in the union of the two maps domains, the function
     * is applied to the image of the key under the map.  If the function
     * returns SOME y, then (k, y) is added to the resulting map.
     *)
    val merge       : ('b option -> 'c option -> 'd option) -> (''a, 'b) map -> (''a, 'c) map -> (''a, 'd) map
    val mergei       : (''a -> 'b option -> 'c option -> 'd option) -> (''a, 'b) map -> (''a, 'c) map -> (''a, 'd) map

    (* Return a map whose domain is the union of the domains of the two input
     * maps, always choosing the second map on elements that are in bot domains.
     *)
    val plus        : (''a, 'b) map -> (''a, 'b) map -> (''a, 'b) map

    val toString    : (''a -> string) -> ('b -> string) -> (''a, 'b) map -> string
end
