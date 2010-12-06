signature Wrap =
sig
  type ('a, 'b) t = {node: 'a, left: 'b, right: 'b}

  val wrap : 'a -> 'b -> 'b -> ('a, 'b) t
  val unwrap : ('a, 'b) t -> 'a
  val left : ('a, 'b) t -> 'b
  val right : ('a, 'b) t -> 'b
  val modify : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
  val extend : ('b -> 'c -> 'd) -> ('a, 'b) t -> 'c -> 'c -> ('a, 'd) t
end
