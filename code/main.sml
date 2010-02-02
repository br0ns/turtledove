val print = fn s => print (s ^ "\n")

(* ;print "[Yeah baby!]"; *)

(* val file = SourceText.fromFile "SourceText.sml" *)
(* ;print (SourceText.getSource file 40 522); *)

(* val (mlb, comments) = MLBParser.parse (hd (CommandLine.arguments ())) *)
(* ;Report.print (MLBGrammar.showBasdecs mlb); *)

val here = Path.new (OS.FileSys.getDir ())
val mlbpath = Path.new' here (hd (CommandLine.arguments ()))

;Benchmark.start ();
val basdecs = MLBParser.fromFile mlbpath
    handle MLBParser.Parse r => (Report.print r ; nil)
         | Path.Path r => (Report.print r ; nil)
;Benchmark.stop ();
;Benchmark.print "Parsing MLB file:";

(* ;Report.print (AstMLB.showBasdecs basdecs); *)

local
structure Set = OrderedSetFn (
                struct
                type t = Path.t
                fun compare x y = String.compare (Path.path x, Path.path y)
                val toString = Path.toString
                end)

(* structure Set = TrieOrderedSetFn ( *)
(*                 OrderedMapFn( *)
(*                 struct *)
(*                 type t = char *)
(*                 fun compare x y = Char.compare (x, y) *)
(*                 val toString = Char.toString *)
(*                 end)) *)

(* structure Set = OrderedSetFn ( *)
(*                 struct *)
(*                 type t = char list *)
(*                 fun compare x y = List.collate Char.compare (x, y) *)
(*                 fun toString _ = "list" *)
(*                 end) *)
in
(* Its much faster to just build a huge list of files and then calling fromList
   on that, than it is to incrementially build the set *)
fun sources ds =
    let
      open AstMLB Set

      val files = ref empty
      fun ignore file =
          String.isSuffix "MLB.lex.sml" (Path.path file) orelse
          String.isSuffix "MLB.yacc.sig" (Path.path file) orelse
          String.isSuffix "MLB.yacc.sml" (Path.path file) orelse
          String.isSuffix "SML.lex.sml" (Path.path file) orelse
          String.isSuffix "SML.yacc.sig" (Path.path file) orelse
          String.isSuffix "SML.yacc.sml" (Path.path file) orelse
          file = Path.new "$(SML_LIB)/basis/basis.mlb" orelse
          file = Path.new "$(SML_LIB)/smlnj-lib/Util/smlnj-lib.mlb" orelse
          file = Path.new "$(SML_LIB)/mlyacc-lib/mlyacc-lib.mlb" orelse
          not (Path.sub (Path.dir mlbpath) file)

      fun basdecs ds = List.app basdec ds
      and basdec d =
          case d of
            Basis bs => basbinds bs
          | Local (ds, ds') => (basdecs ds ; basdecs ds')
          | Include (f, ds) => if ignore f then
                                 ()
                               else
                                 basdecs ds
          | Source f => if ignore f then
                          ()
                        else
                          (* files := insert (!files) (explode (Path.path f)) *)
                          files := insert (!files) f
          | Ann (_, ds) => basdecs ds
          | _ => ()
      and basexp e =
          case e of
            Bas ds => basdecs ds
          | Let (ds, e) => (basdecs ds ; basexp e)
          | Var _ => ()
      and basbinds bs = List.app basbind bs
      and basbind (_, e) = basexp e
    in
      basdecs ds ;
      toList (!files)
      @
      (if Path.file mlbpath = "Turtledove.mlb" then
         [Path.new' here "parsers/mlb/MLB.lex",
          Path.new' here "parsers/mlb/MLB.yacc",
          Path.new' here "parsers/sml/SML.lex",
          Path.new' here "parsers/sml/SML.yacc"]
       else
         nil)
    end
end

;Benchmark.start ();
val srcs = sources basdecs
;Benchmark.stop ();
;Benchmark.print "Collecting source files:";

val srcs = map (fn f => (f, File.size f)) srcs
val srcs = ListSort.sort (fn (_, x) => fn (_, y) => Int.compare (x, y)) srcs

val sz = foldl (fn ((_, s), a) => a + s) 0 srcs

val r = Report.++ (
        Report.itemize
            (map (fn (f, s) =>
                     Report.text (Int.toString s ^ " " ^ Path.path' (Path.dir mlbpath) f)
                 ) srcs
            ),
        Report.text (Int.toString (sz div 1000) ^ "KB in " ^ Int.toString (length srcs) ^ " files.")
        )

;Report.print r;


;Benchmark.start ();
val _ = SMLParser.fromFile (Path.new' here "parsers/sml/SML.yacc.sml")
    handle SMLParser.Parse r => (Report.print r ; raise Fail "[Foobar]")
;Benchmark.stopAndPrint "Parsing parsers/sml/SML.yacc.sml:";

;Benchmark.start ();
val report = SMLParser.fromFile (Path.new' here "Report.sml")
    handle SMLParser.Parse r => (Report.print r ; raise Fail "[Foobar]")
;Benchmark.stopAndPrint "Parsing Report.sml:";

local
  open Tree.Walk SMLGrammar
  structure Set = StringOrderedSet
  val unwrap = Wrap.unwrap
  fun loop (w, s) =
      foldl loop (case unwrap (this w) of
                    Exp_Var i => (Set.insert s o Ident.toString o unwrap) i
                  | _ => s)
            (children w)
  val ids = loop (init report, Set.empty)
in
val _ = print (Set.toString String.toString ids)
val _ = print (Int.toString (Set.card ids))
val _ = print (Int.toString (Tree.size report))
end

val ast = SMLParser.fromFile (Path.new' here "dummy.sml")
val _ = (Report.print o SMLGrammar.show) ast
val (ast, _) = Infixing.resolve (ast, Dictionary.empty)
val _ = (Report.print o SMLGrammar.show) ast

