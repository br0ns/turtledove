signature Source =
sig
    type t
    val fromSourceText : SourceText.t -> t
    val error : t -> int -> string -> 'a
    val makeReader : t -> int -> string

    structure Comments : sig
        type source
        (* position_start * comment *)
        type t = (int * string) list

        val new : source -> int -> unit
        val inc : source -> unit
        val dec : source -> unit
        val append : source -> string -> unit
        val get : source -> t
        val start : source -> int
        val depth : source -> int
    end where type source = t

    structure String : sig
        type source
        val new : source -> int -> unit
        val append : source -> string -> unit
        val appendChar : source -> char -> unit
        val appendControlChar : source -> string -> (string -> 'a) -> unit
        val appendAsciiChar : source -> string -> (string -> 'a) -> unit
        val appendUnicodeChar : source -> string -> (string -> 'a) -> unit
        val get : source -> string
        val start : source -> int
    end where type source = t

end
