

signature Array =
sig

    type 'a t

    val empty  : 'a t

    val get    : 'a t -> int -> 'a t option
    val set    : 'a t -> int -> 'a -> 'a t
    val change : 'a t -> int -> ('a -> 'a) -> 'a t

    val append : 'a t -> 'a -> 'a t

    val length : 'a t -> int

end
