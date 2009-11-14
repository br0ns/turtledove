

signature Queue =
sig
    type 'a queue

    val empty      : 'a queue
    val push       : 'a queue -> 'a -> 'a queue
    val pushList   : 'a queue -> 'a list -> 'a queue
    val append     : 'a queue -> 'a queue -> 'a queue
    val isEmpty    : 'a queue -> bool
    val size       : 'a queue -> int
    val pop        : 'a queue -> 'a
    val peek       : 'a queue -> 'a

    val toString   : ('a -> string) -> 'a queue -> string
end
