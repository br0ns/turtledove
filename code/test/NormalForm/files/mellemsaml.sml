fun mellemsaml [] = ""
  | mellemsaml [x] = x
  | mellemsaml (x::xs) = x ^ " " ^ (mellemsaml xs)
