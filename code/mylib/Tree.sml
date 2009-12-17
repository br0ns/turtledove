(* Default Tree implementation *)

structure Tree = MapTreeFn (
                 ListOrderedMapFn (struct
                                   type t = int
                                   fun compare x y = Int.compare (x, y)
                                   end)
                 )
