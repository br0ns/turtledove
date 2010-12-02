signature Wrap =
sig
  type ('a, 'b) t = {node: 'a, left: 'b, right: 'b}

  val wrap : 'a -> 'b -> 'b -> ('a, 'b) t
  val unwrap : ('a, 'b) t -> 'a
  val left : ('a, 'b) t -> 'b
  val right : ('a, 'b) t -> 'b
  val modify : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
  val rewrap : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
end
