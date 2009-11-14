(* Default Tree implementation *)

structure Tree = TreeFn (
                 ListOrderedMapFn (struct
                                   type domain = int
                                   fun compare x y = Int.compare (x, y)
                                   end)
                 )
