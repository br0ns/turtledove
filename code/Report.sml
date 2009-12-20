(* NOT FINISHED *)

structure Report :> Report =
struct
    datatype post =
             Text of string
           | Verb of string
           | Indent of t
           | Itemize of string * t list
           | Frame of t
           | Row of t list

    withtype t = post list

    fun verbatim s = [Text s]

    fun indent r = [Indent r]

    fun text s = verbatim (TextUtils.wordWrap Constants.LINE_LENGTH s)

    val ++ = op @

    fun toString nil = ""
      | toString [p] = postToString p
      | toString (p :: ps) = postToString p ^ "\n" ^ toString ps

    and postToString (Text s) = s
      | postToString (Indent r) = TextUtils.indent Constants.TAB_WIDTH (toString r)
      | postToString _ = "<Unimplemented>"

    val print = fn r => print (toString r ^ "\n")
end
