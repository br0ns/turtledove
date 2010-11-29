signature SourceText =
sig
    type t

    val fromString : string -> t
    val fromFile : File.t -> t
    val reread : t -> t
    val write : t -> unit

    val getSource : t -> int -> int -> string
    val getFile : t -> Path.t
    val getSize : t -> int

    (* TODO:
     * val source : t -> int -> int -> string
     * val file : t -> Path.t
     * val size : t -> int
     *)

    val patch : t -> int -> int -> string -> t
    val patchLine : t -> int -> string -> t

    val makeReader : t -> int -> string

    val posToRowCol : t -> int -> {row : int, column : int}
    val posToString : t -> int -> string
    val showPos : t -> int -> Layout.t

    val toString : t -> string
end
