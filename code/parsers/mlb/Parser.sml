structure Parser =
struct
open MLBGrammar

structure Map = Path.Map
structure Set = Path.Set

type ast = (File.t, unit, unit) MLBGrammar.ast

structure MLBLrVals = MLBLrValsFun
                        (structure Token = LrParser.Token)
structure MLBLex    = MLBLexFun
                        (structure Tokens = MLBLrVals.Tokens)
structure MLBParser = JoinWithArg
                        (structure ParserData = MLBLrVals.ParserData
                         structure Lex = MLBLex
                         structure LrParser = LrParser)

exception Error of Path.t * string
exception LexError = LexError
exception YaccError = YaccError

fun fromFile file =
    let
      fun isX x f = List.exists
                      (fn e =>
                          case Path.extension f of
                            NONE => false
                          | SOME e' => e = e')
                      x
      val isProg = isX ["ML", "sml", "sig", "fun"]
      val isMLB = isX ["mlb"]

      fun loop (file, seen, parsed) =
          case Map.lookup parsed file of
            SOME x => (x, parsed)
                      before Crash.debug ("Avoided parsing " ^ Path.toString file)
          | NONE =>
            let
              fun fail e = raise Error (file, e)
              val dir = Path.dir file
              val data = SourceData.new ()
              val is = File.openIn file
              val reader = fn _ => TextIO.input is

              open Tree
              fun node t = Wrap.unwrap $ this t
              fun leaf n = Tree.singleton $ Wrap.wrap n () ()

              (* The parser parses until EOF so the rest of the stream is always
               * empty
               *)
              val ((ast, comments), _) =
                  MLBParser.parse (Constants.PARSE_LOOKAHEAD,
                                   MLBParser.makeLexer reader data,
                                fn (s, l, r) => ParserUtils.fail l s,
                                   data)

              fun loop' (t, parsed) =
                  case node t of
                    Dec_Source f =>
                    let
                      val file = Path.new' dir f
                          handle Path.Path e => fail e
                    in
                      if isProg file then
                        (leaf $ Dec_Source file, parsed)
                      else if isMLB file then
                        if Set.member seen file then
                          fail ("Recursive include: " ^ Path.toString file)
                        else
                          let
                            val ({ast, comments}, parsed) =
                                loop (file, Set.insert seen file, parsed)
                          in
                            (leaf
                               $ Dec_Include {file     = file,
                                              ast      = ast,
                                              comments = comments,
                                              basis    = ()},
                             parsed)
                          end
                      else
                        fail ("Unknown file type: " ^ Path.toString file)
                    end
                  | n =>
                    let
                      fun loop'' (nil, parsed) = (nil, parsed)
                        | loop'' (t :: ts, parsed) =
                          let
                            val (t', parsed) = loop' (t, parsed)
                            val (ts', parsed) = loop'' (ts, parsed)
                          in
                            (t' :: ts', parsed)
                          end
                      val (ts', parsed) = loop'' (children t, parsed)
                    in
                      (join (identity $ this t) ts', parsed)
                    end

              val (ast, parsed) = loop' (ast, parsed)
              val this = {ast = ast, comments = comments}
            in
              (this, Map.update parsed (file, this))
            end

      val (res, _) = loop (file, Set.empty, Map.empty)
    in
      res
    end
end
