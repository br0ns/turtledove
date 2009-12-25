val print = fn s => print (s ^ "\n")

(* ;print "[Yeah baby!]"; *)

(* val file = SourceText.fromFile "SourceText.sml" *)
(* ;print (SourceText.getSource file 40 522); *)

(* val (mlb, comments) = MLBParser.parse (hd (CommandLine.arguments ())) *)
(* ;Report.print (MLBGrammar.showBasdecs mlb); *)

fun sources path =
    let
      open MLBGrammar
      open Set
      val dir = Path.dir path

      fun read f path =
          if String.isPrefix "$" path then
            empty
          else
            let
              val path = Path.new' dir path
            in
              if
                List.exists (fn f => f = Path.file path)
                            ["MLB.lex.sml", "MLB.yacc.sig", "MLB.yacc.sml"]
                orelse Path.file (Path.dir path) = "ml-yacc-lib" then
                empty
              else
                f path
            end

      fun basdecs ds = foldl (fn (s, a) => union s a) empty (List.map basdec ds)
      and basdec d =
          case d of
            Basis bs => basbinds bs
          | Local (ds, ds') => union (basdecs ds) (basdecs ds')
          | Include f => read sources f
          | Source f => read singleton f
          | Open _ => empty
      and basexp e =
          case e of
            Bas ds => basdecs ds
          | Let (ds, e) => union (basdecs ds) (basexp e)
          | Var _ => empty
      and basbinds bs = foldl (fn (s, a) => union s a) empty (List.map basbind bs)
      and basbind (_, e) = basexp e
    in
      let
        val (mlb, _) = MLBParser.parse (Path.path path)
      in
        union
        (if Path.file dir = "parsers" then
           fromList [Path.new' dir "MLB.lex", Path.new' dir "MLB.yacc"]
         else
           empty)
        (basdecs mlb)
      end
    end

val here = Path.new (OS.FileSys.getDir ())
val mlbpath = Path.new' here (hd (CommandLine.arguments ()))
val srcs = map (fn f => (f, Path.size f))
               (Set.toList (sources mlbpath))
val srcs = ListSort.sort (fn (_, x) => fn (_, y) => Int.compare (x, y)) srcs

val sz = foldl (fn ((_, s), a) => a + s) 0 srcs

val r = Report.++ (
        Report.itemize
            (map (fn (f, s) =>
                     Report.text (Int.toString s ^ " " ^ Path.path' here f)
                 ) srcs
            ),
        Report.text (Int.toString sz)
        )

;Report.print r;
