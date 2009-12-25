signature MLBParser =
sig
    val fromFile : string -> Ast.MLB.basdecs
    val fromText : string -> Ast.MLB.basdecs
end
