fun mapfil (f, p, []) = []
  | mapfil (f, p, (x::xs)) = 
      (if p x then f x else x) 
      :: mapfil(f, p, xs)
