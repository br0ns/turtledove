(* General lexing utilities. *)

signature LexUtils =
sig
    val isQualStar : string -> bool
    val asQualId : string -> string list
    val asDigit : string -> int
    val asInteger : string -> IntInf.int option
    val asWord : string -> Word32.word option
    val asReal : string -> string option
end
