structure Report : Report =
struct
    datatype post =
             Text of string
           | Indent of report

    withtype report = post list

    fun verbatim s = [Text s]

    fun indent r = [Indent r]

    fun text s = verbatim (TextUtils.wordWrap Constants.LINE_LENGTH s)

    val ++ = op @

    fun toString nil = ""
      | toString [p] = postToString p
      | toString (p :: ps) = postToString p ^ "\n" ^ toString ps

    and postToString (Text s) = s
      | postToString (Indent r) = TextUtils.indent Constants.TAB_WIDTH (toString r)
end
