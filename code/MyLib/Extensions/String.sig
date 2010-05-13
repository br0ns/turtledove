signature String =
sig
  include STRING
  where type string = string
  where type char = Char.char

val tabulate : int * (int -> char) -> string

(* Max width -> text -> wordwrapped text *)
val wordwrap : int -> string -> string

(* Tab width -> text with tabs -> text without tabs *)
val untabify : int -> string -> string

val spaces : int -> string
end
