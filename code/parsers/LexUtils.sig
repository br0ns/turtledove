(* General lexing utilities. String and comment management. *)

signature LexUtils =
sig
    val isQualStar : string -> bool
    val asQualId : string -> string list
    val asDigit : string -> int
    val asInteger : string -> IntInf.int option
    val asWord : string -> Word32.word option
    val asReal : string -> string option

    structure Comments : sig
        type comments

        val clear : unit -> unit
        val new : int -> unit
        val inc : unit -> unit
        val dec : unit -> unit
        val append : string -> unit
        val get : unit -> comments
        val depth : unit -> Int
    end
                         
    structure String : sig
        val clear : unit -> unit
        val append : string -> unit
        val appendChar : char -> unit
        val appendControlChar : string * (string -> 'a) -> unit
        val appendAsciiChar : string * (string -> 'a) -> unit
        val appendUnicodeChar : string * (string -> 'a) -> unit
        val get : unit -> string
    end
end
