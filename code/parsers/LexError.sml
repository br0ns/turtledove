structure LexError =
struct
exception LexError of report

fun raise st pos msg =
        let
            val r = SourceText.posToReport st pos
            val ++ = Report.++
            infix ++
        in
            raise LexError (
                  Report.text "Lexical error:" ++
                  Report.indent (Report.text msg) ++
                  Report.text "at" ++
                  Report.indent r
                  )
        end
end
