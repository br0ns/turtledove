(* TODO: empty productions mess with the position information. Fix that. *)
structure Parser =
struct
type ast = (Grammar.ident, Grammar.ident, int) Grammar.ast

structure SMLLrVals = SMLLrValsFun
                        (structure Token = LrParser.Token)

structure SMLLex    = SMLLexFun
                        (structure Tokens = SMLLrVals.Tokens)

structure SMLParser = JoinWithArg
                        (structure ParserData = SMLLrVals.ParserData
                         structure Lex = SMLLex
                         structure LrParser = LrParser)

exception LexError = LexError
exception YaccError = YaccError

fun fromReader reader =
    let
      val data = SourceData.new ()
      (* The parser parses until EOF so the rest of the stream is always empty *)
      val ((ast, comments), _) =
          SMLParser.parse (Constants.PARSE_LOOKAHEAD,
                           SMLParser.makeLexer reader data,
                        fn (s, l, r) => ParserUtils.fail l s,
                           data)
    in
      {ast = ast, comments = comments}
    end

(* fun fromFile file = *)
(*     let *)
(*       open LazyList *)
(*       val l = ref $ map str $ fromFile $ Path.toString file *)
(*       val reader = *)
(*        fn _ => case getItem (!l) of *)
(*                  NONE => "" *)
(*                | SOME (s, l') => (l := l' ; s) *)
(*     in *)
(*       fromReader reader *)
(*     end *)

fun fromFile file =
    let
      val is = File.openIn file
    in
      fromReader (fn _ => TextIO.input is)
      before TextIO.closeIn is
    end

               (* | Path.Path r => fail r *)
               (* | IO.Io {name, cause = OS.SysErr (err, se), ...} => *)
               (*     fail (txt *)
               (*             (case se of *)
               (*                NONE    => err *)
               (*              | SOME m  => OS.errorMsg m *)
               (*             ) && colon & txt name *)
               (*          ) *)
               (* | IO.Io {name, ...} => *)
               (*   fail (txt "Failed to read file:" & txt name) *)

end
