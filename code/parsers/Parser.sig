signature Parser =
sig
  exception Parse of Layout.t

  type ast

  val fromFile : File.t -> {ast : ast, comments : Comments.t}
(* val fromText : string -> {ast : File.t MLBGrammar.node Tree.t, comments : Comments.t} *)
end
