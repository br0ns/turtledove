(* Default OrderedMap implementation *)

(* OrderedMapFn : Ordering -> OrderedMap *)
functor OrderedMapFn (Ord : Ordering) = ListOrderedMapFn (Ord)
