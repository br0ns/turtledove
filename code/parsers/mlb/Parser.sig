signature Parser =
sig
  exception Parse of Report.t
  val fromFile : File.t -> {basdecs : File.t MLBGrammar.basdecs, comments : Source.Comments.t}
(* val fromText : string -> Ast.MLB.node Tree.t *)
end
