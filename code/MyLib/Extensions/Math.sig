signature Math =
sig
include MATH where type real = Real.real

(* faster than pow *)
val intpow : real * int -> real
end
