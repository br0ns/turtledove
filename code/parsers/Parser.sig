signature Parser =
sig
  exception Error of int * string

  type ast

  val fromFile : File.t -> {ast : ast, comments : Comments.t}
(* val fromText : string -> {ast : File.t MLBGrammar.node Tree.t, comments : Comments.t} *)
end
