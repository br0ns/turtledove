

signature Array =
sig

    type 'a array

    val empty  : 'a array

    val get    : 'a array -> int -> 'a array option
    val set    : 'a array -> int -> 'a -> 'a array
    val change : 'a array -> int -> ('a -> 'a) -> 'a array

    val append : 'a array -> 'a -> 'a array

    val length : 'a array -> int

end
