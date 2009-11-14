structure PiLrVals = PiLrValsFun(
              structure Token = LrParser.Token);
structure PiLex    = PiLexFun(
              structure Tokens = PiLrVals.Tokens);
structure PiParser = JoinWithArg(
              structure ParserData = PiLrVals.ParserData
              structure Lex=PiLex
              structure LrParser=LrParser);
