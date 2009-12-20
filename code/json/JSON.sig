signature JSON =
sig
    datatype t = Object of t Dictionary.t
               | Array of t list
               | String of string
               | Number of real
               | Bool of bool
               | Null
    exception Parse of Report.t

    (* Reads one JSON value. If the string contains more than a single JSON
       value or is not in a valid JSON format the JSON.Parse exception is
       raised.*)
    val read : string -> t

    (* Same as JSON.read, but can read multiple JSON values. If the string is
       not in valid a valid JSON format the JSON.Parse exception is raised.  The
       JSON values is not required to be seperated by whitespace, but sending
       two numbers without whitespace between will result in them being read as
       one *)
    val readMany : string -> t list

    (* Writes a SML representation of a JSON value (JSON.t) to a string. *) 

    val write : t -> string

    (* Writes a list of JSON values (JSON.t) to a string seperated by newlines *)
    val writeMany : t list -> string

    structure Converter : sig
        type json
        type 'a t
        exception Match
        val make : {toJSON : 'a -> json, fromJSON : json -> 'a} -> 'a t
        val object : 'a t -> 'a Dictionary.t t
        val array : 'a t -> 'a list t
        val string : string t
        val number : real t
        val bool : bool t
        val null : unit t
        val json : json t
    end where type json = t

    (* Converts from a string to a SML representation of a JSON value (JSON.t) given the
       specified Converter *)
    val from : 'a Converter.t -> string -> 'a

    (* Converts from a string containing multiple JSON values to a list of the
       SML representation (JSON.t) given the specified Converter *)
    val fromMany : 'a Converter.t -> string -> 'a list

    (* Converts from a SML representation of a JSON value (JSON.t) to a string
       given the specified Converter *)
    val to : 'a Converter.t -> 'a -> string

    (* Converts from a list of the SML representation of a JSON value (JSON.t)
       to a string, seperated by newlines, given the specified Converter *)
    val toMany : 'a Converter.t -> 'a list -> string

end
