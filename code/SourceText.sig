signature SourceText =
sig
    type t

    val fromString : string -> t 
    val fromFile : File.t -> t
    val reread : t -> t

    val getSource : t -> int -> int -> string
    val getFile : t -> Path.t
    val getSize : t -> int

    val patch : t -> int -> int -> string -> t
    val patchLine : t -> int -> string -> t

    val makeReader : t -> int -> string

    val posToString : t -> int -> string
    val showPos : t -> int -> Report.t
end
