(* Default OrderedMap implementation *)

(* OrderedMapFn : Ordering -> OrderedMap *)
functor OrderedMapFn (Key : Ordered) = ListOrderedMapFn (Key)
