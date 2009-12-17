signature LexError =
sig
    exception LexError

    val raise : SourceText.source_text -> int -> string -> 'a
end
