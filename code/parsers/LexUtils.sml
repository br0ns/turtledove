structure LexUtils :> LexUtils =
struct
    fun die s = Crash.impossible ("LexUtils: " ^ s)

    fun isQualStar s = die "isQualStar"
    fun asQualId s = die "asQualId"
    fun asDigit s = die "asDigit"
    fun asInteger s = die "asInteger"
    fun asWord s = die "asWord"
    fun asReal s = die "asReal"

    structure Comments =
    struct
        type comments = (int * string) list
        fun die s = Crash.impossible ("Comments: " ^ s)

        val d = ref 0
        val cs = ref nil

        fun clear () = (depth := 0 ; comments := nil)

        fun new p = cs := (p, "") :: cs

        fun inc () = d := !d + 1

        fun dec () = d := !d - 1

        fun append s = case !cs of
                           (p, c) :: cs => cs := (p, c ^ s) :: cs
                         | _ => die "append: No comments opened."

        fun get () = rev !cs

        fun openedAt () = case !cs of
                              (p, _) => p
                            | _ => die "openedAt: No comments opened."

        fun depth () = !d
    end

    structure String =
    struct
        val s = ref ""

        fun clear () = s := ""

        fun append cs = s := !s ^ cs
        val appendChar = append o str

        fun asControlChar s fail =
            case explode s of
                [#"\\", c] =>
                let
                    val c = ord c
                in
                    if c >= ord #"@" andalso c <= ord #"_" then
                        chr (c - ord #"@")
                    else
                        fail "Illegal control escape; must be one of " ^
                             "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
                end
              | _ => die "asControlChar"

        fun charToInt c =
            if Char.isDigit c then
                ord c - ord #"0"
            else if Char.isUpper c then
                ord c - ord #"A" + 10
            else if Char.isLower c then
                ord c - ord #"a" + 10
            else
                die "charToInt"

        fun charsToInt base =
            foldl (fn (c, a) => a * base + charToInt c) 0

        fun asAsciiChar s fail =
            case explode s of
                [#"\\", c1, c2, c3] =>
                let
                    val c = charsToInt 10 [c1, c2, c3]
                in
                    if c > 255 then
                        fail "Illegal ASCII escape"
                    else
                        chr c
                end
              | _ => die "asAsciiChar"

        fun asUnicodeChar s fail =
            case explode s of
                [#"\\", #"u", c1, c2, c3, c4] =>
                let
                    val c = charsToInt 16 [c1, c2, c3, c4]
                in
                    if c > 255 then
                        fail "Illegal unicode escape (characters 0--255 supported)"
                    else
                        chr c
                end
              | _ => die "asUnicodeChar"

        val appendControlChar = appendChar o asControlChar
        val appendAsciiChar = appendChar o asAsciiChar
        val appendUnicodeChar = appendChar o asUnicodeChar

        fun get () = !s
    end
end
