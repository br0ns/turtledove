(* TODO: empty productions mess with the position information. Fix that. *)
structure Parser =
struct
type ast = (Grammar.ident, int) Grammar.ast

structure RuleLrVals = RuleLrValsFun
                        (structure Token = LrParser.Token)

structure RuleLex    = RuleLexFun
                        (structure Tokens = RuleLrVals.Tokens)

structure RuleParser = JoinWithArg
                        (structure ParserData = RuleLrVals.ParserData
                         structure Lex = RuleLex
                         structure LrParser = LrParser)

exception LexError = LexError
exception YaccError = YaccError

fun fromReader reader =
    let
      val data = SourceData.new ()
      (* The parser parses until EOF so the rest of the stream is always empty *)
      val ((ast, comments), _) =
          RuleParser.parse (Constants.PARSE_LOOKAHEAD,
                           RuleParser.makeLexer reader data,
                        fn (s, l, r) => ParserUtils.fail l s,
                           data)
    in
      {ast = ast, comments = comments}
    end

fun fromFile file =
    let
      val is = File.openIn file
    in
      fromReader (fn _ => TextIO.input is)
      before TextIO.closeIn is
    end
end
