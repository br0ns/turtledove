signature List =
sig
  include LIST

  val sort : ('a -> 'a -> order) -> 'a list -> 'a list
  val shuffle : 'a list -> 'a list
  val leftmost  : 'a option list -> 'a option
  val rightmost : 'a option list -> 'a option
  val allPairs : 'a list -> 'b list -> ('a * 'b) list
end
