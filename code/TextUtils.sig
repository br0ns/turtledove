signature TextUtils =
sig
    val indent : int -> string -> string
    val decorate : string -> string -> string
    val wordWrap : int -> string -> string
    val untabify : string -> string
end
