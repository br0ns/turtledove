
signature Ordered =
sig
    eqtype t
    val compare  : t -> t -> order

    (* Needed for toString and show functions *)
    val toString : t -> string
end
