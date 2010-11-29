(* TODO: empty productions mess with the position information. Fix that. *)
structure Parser :> Parser =
struct

structure RuleLrVals = RuleLrValsFun
                        (structure Token = LrParser.Token)

structure RuleLex    = RuleLexFun
                        (structure Tokens = RuleLrVals.Tokens)

structure RuleParser = JoinWithArg
                        (structure ParserData = RuleLrVals.ParserData
                         structure Lex = RuleLex
                         structure LrParser = LrParser)


exception Parse of Layout.t

type ast = Grammar.ast

fun fromSourceText st =
    let
      open Layout
      infix ^^ ++ \ & \\ &&
      val file = SourceText.getFile st
      fun fail e = raise Parse (txt "Error while parsing" & txt (Path.path file) && colon \ indent 2 e)
    in
      let
        val source = Source.fromSourceText st
        val reader = Source.makeReader source

        (* The parser parses until EOF so the rest of the stream is always empty *)
        val ((ast, comments), _) =
            RuleParser.parse (Constants.PARSE_LOOKAHEAD,
                             RuleParser.makeLexer reader source,
                          fn (s, l, r) => Source.error source l s,
                             source)
      in
        {ast = ast, comments = comments}
      end handle LexError r => fail r
               | Path.Path r => fail r
               | IO.Io {name, cause = OS.SysErr (err, se), ...} =>
                   fail (txt
                           (case se of
                              NONE    => err
                            | SOME m  => OS.errorMsg m
                           ) && colon & txt name
                        )
               | IO.Io {name, ...} =>
                 fail (txt "Failed to read file:" & txt name)
               | ParseError (p, s) =>
                 raise Parse (
                       txt "Error at" &
                           SourceText.showPos st p && colon \ indent 2 (txt s)
                       )
    end

val fromFile = fromSourceText o SourceText.fromFile

end
