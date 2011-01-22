fun udvaelg xs [] = []
  | udvaelg xs (y::ys) = List.nth(xs,y) :: udvaelg xs ys
