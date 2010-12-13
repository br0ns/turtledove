signature SourceData =
sig
    type t
    val new : unit -> t

    structure Comments : sig
        type source_data
        (* position_start * comment *)
        type t = (int * string) list

        val new : source_data -> int -> unit
        val inc : source_data -> unit
        val dec : source_data -> unit
        val append : source_data -> string -> unit
        val get : source_data -> t
        val start : source_data -> int
        val depth : source_data -> int
    end where type source_data = t

    structure String : sig
        type source_data
        val new : source_data -> int -> unit
        val append : source_data -> string -> unit
        val appendChar : source_data -> char -> unit
        val appendControlChar : source_data -> string -> (string -> 'a) -> unit
        val appendAsciiChar : source_data -> string -> (string -> 'a) -> unit
        val appendUnicodeChar : source_data -> string -> (string -> 'a) -> unit
        val get : source_data -> string
        val start : source_data -> int
    end where type source_data = t

end
