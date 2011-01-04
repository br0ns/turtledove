signature SCon =
sig
datatype t = String of string
           | Char of string
           | Int of string
           | Real of string
           | Word of string
val toString : t -> string
end
