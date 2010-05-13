structure LexUtils :> LexUtils =
struct
    fun die s = Crash.impossible ("LexUtils: " ^ s)

    fun isQualStar s = die "isQualStar"
    fun asQualId s = die "asQualId"
    fun asDigit s = die "asDigit"
    fun asInteger s = die "asInteger"
    fun asWord s = die "asWord"
    fun asReal s = die "asReal"
end
