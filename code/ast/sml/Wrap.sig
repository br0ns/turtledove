signature Wrap =
sig
  type ('a, 'b) t = {node: 'a, data: 'b}

  val wrap : 'a -> 'b -> ('a, 'b) t
  val unwrap : ('a, 'b) t -> 'a
  val data : ('a, 'b) t -> 'b
  val modify : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
end
