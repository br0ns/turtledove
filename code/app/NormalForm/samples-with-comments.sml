

(* Eksamen 2010  *)
fun udvaelg xs [] = []
  | udvaelg xs (y::ys) = List.nth(xs,y) :: udvaelg xs ys


(* Bliver til *)

fun udvaelg' xs ys = map (fn y => List.nth (xs,y)) ys;


val tst2010_1 = udvaelg [#"t",#"o",#"n",#"e"] [2,3,0,0,1] = [#"n",#"e",#"t",#"t",#"o"]
val tst2010_2 = udvaelg' [#"t",#"o",#"n",#"e"] [2,3,0,0,1] = [#"n",#"e",#"t",#"t",#"o"]




(* Eksamen 2009. Omskriv følgende ved brug af hojereordens funktioner. *)

fun collatz [] = []
  | collatz (x::xs) = if x mod 2 = 0 then 
                        x div 2 :: collatz xs
                      else
                        3 * x + 1 :: collatz xs

(* Kan ikke lade sig gøre uden først at faktorisere ":: collatz" ud så vi
   "simplificerer" den lige *)

fun collatz [] = []
  | collatz (x::xs) = (if x mod 2 = 0 then x div 2 else 3 * x + 1) :: collatz xs

(* Bliver til *)

fun collatz' xs =  map (fn x => if (x mod 2 = 0) then x div 2 else (x*3)+1) xs;



(* Eksamen 2008 *)

fun composelist [] b = b
  | composelist (x::xs) b = x (composelist xs b);

(* bliver til *)

fun composelist' []   x = x
  | composelist' list x = foldr (fn (funk, b) => funk b) x list;

val tst2008_1 = composelist [fn y => y*y, fn z => z+1, fn w => w div 2] 4 = 9;
val tst2008_2 = composelist' [fn y => y*y, fn z => z+1, fn w => w div 2] 4 = 9



fun sumlige [] = 0
  | sumlige (x :: xs) = 
    if x mod 2 = 0 then 
      x div 2 + sumlige xs 
    else 
      sumlige xs
      
(* Skal også lige "simplificeres" *)

fun sumlige [] = 0
  | sumlige (x :: xs) = (if x mod 2 = 0 then x div 2 else 0) + sumlige xs

(* bliver til *)

fun sumlige xs = foldl (fn (a,b) => if a mod 2 = 0 then a div 2 + b else 0 + b) 0 xs

(* Kan ikke fanges *)

fun mellemsaml [] = ""
  | mellemsaml [x] = x
  | mellemsaml (x::xs) = x ^ " " ^ (mellemsaml xs)



fun mellemsaml' [] = ""
  | mellemsaml' (t::ts) = foldl (fn (a,b) => b^" "^a) t ts;
