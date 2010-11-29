signature Parser =
sig
  exception Parse of Layout.t
  val fromFile : File.t -> {ast : File.t MLBGrammar.node Tree.t, comments : Comments.t}
(* val fromText : string -> {ast : File.t MLBGrammar.node Tree.t, comments : Comments.t} *)
end
