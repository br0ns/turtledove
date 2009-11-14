signature SourceText =
sig
    type source_text

    val fromString : string -> source_text 
    val fromFile : string -> source_text
    val reread : source_text -> source_text

    val getSource : source_text -> int -> int -> string

    val patch : source_text -> int -> int -> string -> source_text
    val patchLine : source_text -> int -> string -> source_text

    val mkLexingFn : source_text -> int -> string

    val posToString : source_text -> int -> string
    val posToReport : source_text -> int -> Report.report
end
