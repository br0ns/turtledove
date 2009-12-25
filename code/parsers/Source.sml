exception LexError of Report.t

structure Source :> Source =
struct
    type comments = (int * string) list
    type t = (int ref * comments ref) * string ref * SourceText.t

    fun fromSourceText st = ((ref 0, ref nil), ref "", st)
    fun lexError (_, _, st) pos msg =
        let
            val r = SourceText.showPos st pos
            open Report
            infix ++
        in
            raise LexError (
                  text "Lexical error:" ++
                  indent (text msg) ++
                  text "at" ++
                  indent r
                  )
        end
    fun makeReader (_, _, st) = SourceText.makeReader st
        
    structure Comments =
    struct
        type source = t
        type t = comments
        fun die s = Crash.impossible ("LexUtils.Comments: " ^ s)

        fun clear ((d, cs), _, _) = (d := 0 ; cs := nil)

        fun new ((d, cs), _, _) p = (d := !d + 1 ; cs := (p, "") :: !cs)

        fun inc ((d, _), _, _) = d := !d + 1

        fun dec ((d, _), _, _) = d := !d - 1

        fun append ((_, cs), _, _) s =
            case !cs of
                (p, c) :: r => cs := (p, c ^ s) :: r
              | _ => die "append: No comments opened."

        fun get ((_, cs), _, _) = rev (!cs)

        fun openedAt ((_, cs), _, _) =
            case !cs of
                (p, _) :: _ => p
              | _ => die "openedAt: No comments opened."

        fun depth ((d, _), _, _) = !d
    end

    structure String =
    struct
        type source = t
        fun die s = Crash.impossible ("LexUtils.String: " ^ s)
        fun clear (_, s, _) = s := ""

        fun append (_, s, _) cs = s := !s ^ cs
        fun appendChar s = append s o str

        fun asControlChar s fail =
            case explode s of
                [#"\\", c] =>
                let
                    val c = ord c
                in
                    if c >= ord #"@" andalso c <= ord #"_" then
                        chr (c - ord #"@")
                    else
                        (fail ("Illegal control escape; must be one of " ^
                              "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_") ;
                         #" " (* Dummy *)
                        )
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
                        (fail "Illegal ASCII escape" ;
                         #" " (* Dummy *)
                        )
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
                        (fail "Illegal unicode escape (characters 0--255 supported)" ;
                         #" " (* Dummy *)
                        )
                    else
                        chr c
                end
              | _ => die "asUnicodeChar"

        fun appendControlChar s c fail = appendChar s (asControlChar c fail)
        fun appendAsciiChar s c fail = appendChar s (asAsciiChar c fail)
        fun appendUnicodeChar s c fail = appendChar s (asUnicodeChar c fail)

        fun get (_, s, _) = !s
    end
end
