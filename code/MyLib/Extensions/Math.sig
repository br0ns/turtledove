signature Math =
sig
include MATH where type real = Real.real

(* faster than pow *)
val intpow : real * int -> real

val mean : real list -> real
val harmonicMean : real list -> real
end
