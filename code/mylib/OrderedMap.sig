(* Finite map data structure with ordered keys *)

signature OrderedMap =
sig
    eqtype key
    type 'a map

    val empty       : 'a map

    val singleton   : key * 'a -> 'a map

    (* Insert if key is not in map *)
    val insert      : 'a map -> key * 'a -> 'a map option
    val fromList    : (key * 'a) list -> 'a map

    val delete      : 'a map -> key -> 'a map
    val update      : 'a map -> key * 'a -> 'a map
    val change      : 'a map -> key -> ('a -> 'a) -> 'a map option
    val updateList  : 'a map -> (key * 'a) list -> 'a map
    val lookup      : 'a map -> key -> 'a option

    val inDomain    : 'a map -> key -> bool
    val isEmpty     : 'a map -> bool

    val size        : 'a map -> int

    val toList      : 'a map -> (key * 'a) list
    val domain      : 'a map -> key list
    val range       : 'a map -> 'a list

    (* May raise Empty *)
    val first       : 'a map -> 'a
    val firsti      : 'a map -> key * 'a
    val last        : 'a map -> 'a
    val lasti       : 'a map -> key * 'a

    val split       : 'a map -> ((key * 'a) * 'a map) option
    val splitFirst  : 'a map -> ((key * 'a) * 'a map) option
    val splitLast   : 'a map -> ((key * 'a) * 'a map) option

    val collate     : ('a -> 'a -> order) -> 'a map -> 'a map -> order

    val partition   : ('a -> bool) -> 'a map -> 'a map * 'a map
    val partitioni  : (key * 'a -> bool) -> 'a map -> 'a map * 'a map
    val filter      : ('a -> bool) -> 'a map -> 'a map
    val filteri     : (key * 'a -> bool) -> 'a map -> 'a map
    val remove      : ('a -> bool) -> 'a map -> 'a map
    val removei     : (key * 'a -> bool) -> 'a map -> 'a map
    val exists      : ('a -> bool) -> 'a map -> bool
    val existsi     : (key * 'a -> bool) -> 'a map -> bool
    val all         : ('a -> bool) -> 'a map -> bool
    val alli        : (key * 'a -> bool) -> 'a map -> bool
    val find        : ('a -> bool) -> 'a map -> key * 'a
    val findi       : (key * 'a -> bool) -> 'a map -> key * 'a

    val app         : ('a -> unit) -> 'a map -> unit
    val appi        : (key * 'a -> unit) -> 'a map -> unit
    val map         : ('a -> 'b) -> 'a map -> 'b map
    val mapi        : (key * 'a -> 'b) -> 'a map -> 'b map
    val mapPartial  : ('a -> 'b option) -> 'a map -> 'b map
    val mapPartiali : (key * 'a -> 'b option) -> 'a map -> 'b map
    val foldl       : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldli      : ((key * 'a) * 'b) -> 'b -> 'a map -> 'b
    val foldr       : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldri      : ((key * 'a) * 'b) -> 'b -> 'a map -> 'b

    val union       : ('a -> 'a -> 'a) -> 'a map -> 'a map -> 'a map
    val unioni      : (key -> 'a -> 'a -> 'a) -> 'a map -> 'a map -> 'a map 
    (* return a map whose domain is the union of the domains of the two input
     * maps, using the supplied function to define the map on elements that
     * are in both domains.
     *)

    (* return a map whose domain is the intersection of the domains of the
     * two input maps, using the supplied function to define the range.
     *)
    val inter       : ('a -> 'b -> 'c) -> 'a map -> 'b map -> 'c map
    val interi      : (key -> 'a -> 'b -> 'c) -> 'a map -> 'b map -> 'c map

    (* merge two maps using the given function to control the merge. For
     * each key k in the union of the two maps domains, the function
     * is applied to the image of the key under the map.  If the function
     * returns SOME y, then (k, y) is added to the resulting map.
     *)
    val merge       : ('a option -> 'b option -> 'c option) -> 'a map -> 'b map -> 'c map
    val mergi       : (key -> 'a option -> 'b option -> 'c option) -> 'a map -> 'b map -> 'c map

    (* Return a map whose domain is the union of the domains of the two input
     * maps, always choosing the second map on elements that are in bot domains.
     *)
    val plus        : 'a map -> 'a map -> 'a map
                                   
    val toString    : (key -> string) -> ('a -> string) -> 'a map -> string
end
