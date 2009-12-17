signature MLBGrammarUtils =
sig
    val showBId : MLBGrammer.bid -> string
    val showLongBId : MLBGrammer.longbid -> string
    val showFile : MLBGrammer.file -> string
    val showBDec : MLBGrammer.bdec -> string
    val showBExp : MLBGrammer.bexp -> string

    val mkId = Ident.mkId
    val mkLongId = Ident.mkLongId o LexUtils.asQualId
end
