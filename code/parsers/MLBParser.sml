structure MLBParser =
struct
    structure MLBLrVals = MLBLrValsFun
                              (structure Token = LrParser.Token)
    structure MLBLex    = MLBLexFun
                              (structure Tokens = MLBLrVals.Tokens)
    structure MLBParser = JoinWithArg
                          (structure ParserData = MLBLrVals.ParserData
                           structure Lex = MLBLex
                           structure LrParser = LrParser)
                          
    fun parse file =
        let
            val st = SourceText.fromFile file
            val source = Source.fromSourceText st
            val reader = Source.makeReader source
            val (mlb, _) = MLBParser.parse
                           (Constants.PARSE_LOOKAHEAD,
                            MLBParser.makeLexer reader source,
                         fn (s, l, r) => Source.lexError source l s,
                            source)
        in
            mlb
        end handle e as LexError r => (Report.print r ; raise e)
end
