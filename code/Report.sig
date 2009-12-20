signature Report =
sig
    type t

    val text : string -> t
    val indent : t -> t
    val verbatim : string -> t
    val ++ : t * t -> t
    val toString : t -> string
    val print : t -> unit
end
