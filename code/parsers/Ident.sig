signature Ident =
sig
    eqtype id
    val idToString : id -> string
    val showId : id -> string

    eqtype longid
    val longIdToString : longid -> string
    val showLongId : longid -> string

    val mkId : string -> id
    val inventId : unit -> id
    val mkLongId : string list -> longid
    val inventLongId : unit -> longid
    val idToLongId : id -> longid

    val implodeLongId : id list * id -> longid
    val explodeLongId : longid -> id list * id

    val unqualified : longid -> bool
    val longIdToId : longid -> id

    val bogus : longid
end
