(* No std lib, no infix resolving *)

datatype 'a option = SOME of 'a
                   | NONE
exception Option

fun valOf (SOME x) = x
  | valOf NONE = raise Option

fun foo 0 = 1
  | foo n = n * foo (n - 1)

