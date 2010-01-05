signature MLBParser =
sig
    val fromFile : File.t -> Ast.MLB.node Tree.t
    (* val fromText : string -> Ast.MLB.node Tree.t *)
end
