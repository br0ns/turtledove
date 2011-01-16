fun collatz [] = []
  | collatz (x::xs) = (if x mod 2 = 0 then x div 2 else 3 * x + 1) :: collatz xs
