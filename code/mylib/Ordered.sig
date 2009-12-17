
signature Ordered =
sig
    eqtype t
    val compare : t -> t -> order
end
