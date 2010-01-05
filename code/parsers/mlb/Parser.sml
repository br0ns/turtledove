structure Parser =
struct
structure MLBLrVals = MLBLrValsFun
                        (structure Token = LrParser.Token)
structure MLBLex    = MLBLexFun
                        (structure Tokens = MLBLrVals.Tokens)
structure MLBParser = JoinWithArg
                        (structure ParserData = MLBLrVals.ParserData
structure Lex = MLBLex
structure LrParser = LrParser)
                      
structure X = MLBGrammar
structure Y = AstMLB

structure Ord =
struct
type t = File.t
fun compare p p' = String.compare (Path.toString p, Path.toString p')
val toString = Path.toString
end
structure Map = OrderedMapFn (Ord)
structure Set = OrderedSetFn (Ord)

exception Parse of Report.t

fun fromFile file =
    let
      fun context r = Report.++ (r, Report.text ("while parsing " ^ Path.path file))
      fun fail r = raise Parse (context r)
      fun isX x f = List.exists (fn e =>
                                    case Path.extension f of
                                      NONE => false
                                    | SOME e' => e = e') x
      val isProg = isX ["ML", "sml", "sig", "fun"]
      val isMLB = isX ["mlb"]
      val parsed = ref Map.empty

      fun loop (file, seen) =
          case Map.lookup (!parsed) file of
            SOME x => x before Crash.debug ("Avoided parsing " ^ Path.toString file)
          | NONE =>
            let
              fun context' r =
                  Report.++ (r, context (Report.text ("in " ^ Path.path file)))
              fun fail' s = raise (Parse o context o Report.text) s
              val dir = Path.dir file
              val st = SourceText.fromFile file
              val source = Source.fromSourceText st
              val reader = Source.makeReader source

              (* The parser parses until EOF so the rest of the stream is always empty *)
              val ((ds, comments), _) =
                  MLBParser.parse (Constants.PARSE_LOOKAHEAD,
                                   MLBParser.makeLexer reader source,
                                fn (s, l, r) => Source.lexError source l s,
                                   source)

              fun basdecs ds = map basdec ds
              and basdec d =
                  case d of
                    X.Basis bs => Y.Basis (basbinds bs)
                  | X.Local (ds, ds') => Y.Local (basdecs ds, basdecs ds')
                  | X.File f =>
                    let
                      val file = Path.new' dir f
                          handle Path.Path r => raise Parse (context' r)
                    in
                      if isProg file then
                        Y.Source file
                      else if isMLB file then
                        if Set.member seen file then
                          fail' ("Recursive MLB file: " ^ Path.path file)
                        else
                          Y.Include (file, loop (file, Set.insert seen file))
                      else
                        fail' ("Unknown file type: " ^ Path.path file)
                    end
                  | X.Open ids => Y.Open ids
                  | X.Ann (anns, ds) => Y.Ann (anns, basdecs ds)
                  | X.Structure bs => Y.Structure bs
                  | X.Signature bs => Y.Signature bs
                  | X.Functor bs => Y.Functor bs
                  | X.Prim => Y.Prim
              and basexp e =
                  case e of
                    X.Bas ds => Y.Bas (basdecs ds)
                  | X.Let (ds, e) => Y.Let (basdecs ds, basexp e)
                  | X.Var id => Y.Var id
              and basbinds bs = map basbind bs
              and basbind (id, e) = (id, basexp e)
                                    
              val ds = basdecs ds
            in
              parsed := Map.update (!parsed) (file, ds) ; 
              ds
            end
            handle LexError r => fail r
                 | Path.Path r => fail r
                 | IO.Io {name, cause = OS.SysErr (err, se), ...} =>
                   fail (Report.text (
                         (case se of
                            NONE    => err
                          | SOME m  => OS.errorMsg m
                         ) ^ ": " ^ name)
                        )
                 | IO.Io {name, ...} =>
                   fail (Report.text ("Failed to read file: " ^ name))
    in
      loop (file, Set.empty)
    end
end
