signature JSON =
sig
    datatype t = Object of t Dictionary.t
               | Array of t list
               | String of string
               | Number of real
               | Bool of bool
               | Null
    exception Parse of Report.t

    val read : string -> t
    val readMany : string -> t list
    val write : t -> string
    val writeMany : t list -> string

    structure Converter : sig
        type json
        type 'a t
        val make : {toJSON : 'a -> json, fromJSON : json -> 'a} -> 'a t
        val object : 'a t -> 'a Dictionary.t t
        val array : 'a t -> 'a list t
        val string : string t
        val number : real t
        val bool : bool t
        val null : unit t
        val json : json t
    end where type json = t

    val from : 'a Converter.t -> string -> 'a
    val fromMany : 'a Converter.t -> string -> 'a list
    val to : 'a Converter.t -> 'a -> string
    val toMany : 'a Converter.t -> 'a list -> string

end
