structure Parser : Parser =
struct
open MLBGrammar

structure Ord =
struct
  type t = File.t
  fun compare p p' = String.compare (Path.toString p, Path.toString p')
  val toString = Path.toString
end
structure Map = OrderedMapFn (Ord)
structure Set = OrderedSetFn (Ord)

type ast = (File.t, unit) MLBGrammar.ast

structure MLBLrVals = MLBLrValsFun
                        (structure Token = LrParser.Token)
structure MLBLex    = MLBLexFun
                        (structure Tokens = MLBLrVals.Tokens)
structure MLBParser = JoinWithArg
                        (structure ParserData = MLBLrVals.ParserData
                         structure Lex = MLBLex
                         structure LrParser = LrParser)

exception Parse of Layout.t

fun node t = Wrap.unwrap $ Tree.this t
fun wrap n = Wrap.wrap n () ()
fun join n ts = Tree.join (wrap n) ts
fun leaf n = Tree.singleton $ wrap $ n

fun fromFile file =
    let
      open Layout
      infix ^^ ++ \ & \\ &&

      fun fail e = raise Parse (txt "Error while parsing" & txt (Path.path file)
                                    && colon \ indent 2 e)
      fun isX x f = List.exists
                      (fn e =>
                          case Path.extension f of
                            NONE => false
                          | SOME e' => e = e')
                      x
      val isProg = isX ["ML", "sml", "sig", "fun"]
      val isMLB = isX ["mlb"]
      val parsed = ref Map.empty

      fun loop (file, seen) =
          case Map.lookup (!parsed) file of
            SOME x => x before Crash.debug ("Avoided parsing " ^ Path.toString file)
          | NONE =>
            let
              fun fail e = raise Parse (txt "Error in" & txt (Path.path file) && colon \ indent 2 e)
              val dir = Path.dir file
              val st = SourceText.fromFile file
              val source = Source.fromSourceText st
              val reader = Source.makeReader source

              (* The parser parses until EOF so the rest of the stream is always empty *)
              val ((ast, comments), _) =
                  MLBParser.parse (Constants.PARSE_LOOKAHEAD,
                                   MLBParser.makeLexer reader source,
                                fn (s, l, r) => Source.error source l s,
                                   source)

              open Tree

              fun loop' t =
                  case node t of
                    Dec_Source f =>
                    let
                      val file = Path.new' dir f
                          handle Path.Path e => fail e
                    in
                      if isProg file then
                        leaf $ Dec_Source file
                      else if isMLB file then
                        if Set.member seen file then
                          fail (txt "Recursive MLB file:" & txt (Path.path file))
                        else
                          let
                            val {ast, comments} = loop (file, Set.insert seen file)
                          in
                            leaf
                              $ Dec_Include {file     = file,
                                             ast      = ast,
                                             comments = comments}
                          end
                      else
                        fail (txt "Unknown file type:" & txt (Path.path file))
                    end
                  | n => join (identity $ Tree.this t) $ List.map loop' $ children t
              val this = {ast = loop' ast, comments = comments}
            in
              parsed := Map.update (!parsed) (file, this) ;
              this
            end
            handle LexError e => fail e
                 | Path.Path e => fail e
                 | IO.Io {name, cause = OS.SysErr (err, se), ...} =>
                   fail (txt
                           (case se of
                              NONE    => err
                            | SOME m  => OS.errorMsg m
                           ) && colon & txt name
                        )
                 | IO.Io {name, ...} =>
                   fail (txt "Failed to read file:" & txt name)
    in
      loop (file, Set.empty)
    end
end
