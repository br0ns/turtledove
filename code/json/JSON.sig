signature JSON =
sig
    type t
    exception Parse of string * string

    val read : string -> t
    val readMany : string -> t list
    val write : t -> string
    val writeMany : t list -> string

    type 'a converter = (t -> 'a) * ('a -> t)
    val from : 'a converter -> string -> 'a
    val fromMany : 'a converter -> string -> 'a list
    val to : 'a converter -> 'a -> string
    val toMany : 'a converter -> 'a list -> string

    structure Convert : sig
        exception Match
        val object : 'a converter -> 'a Dictionary.t converter
        val array : 'a converter -> 'a list converter
        val string : string converter
        val number : real converter
        val bool : bool converter
        val null : unit converter
        val json : t converter
    end
end
