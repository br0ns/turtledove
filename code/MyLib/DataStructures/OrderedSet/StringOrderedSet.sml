structure StringOrderedSet = OrderedSetFn (struct
                                           type t = string
                                           fun compare x y = String.compare (x, y)
                                           fun toString x = x
                                           end)
