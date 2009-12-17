signature Multiset =
sig

    eqtype 
    eqtype ''a multiset

    val empty : ''a multiset

    val insert : ''a multiset -> ''a -> ''a multiset
end

