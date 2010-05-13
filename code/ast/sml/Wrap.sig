signature Wrap =
sig
eqtype ''a t

val wrap : ''a -> int -> int -> ''a t
val unwrap : ''a t -> ''a
val left : ''a t -> int
val right : ''a t -> int
val modify : (''a -> ''b) -> ''a t -> ''b t
end
