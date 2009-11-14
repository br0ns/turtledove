
signature Ordering =
sig
    eqtype domain
    val compare : domain -> domain -> order
end
