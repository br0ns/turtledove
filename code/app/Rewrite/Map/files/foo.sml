fun foo nil = nil 
  | foo (x :: xs) = x + 1 :: foo xs 
