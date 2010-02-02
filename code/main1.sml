val print = fn s => print (s ^ "\n")

;print "[Yeah baby!]";

val here = Path.new (OS.FileSys.getDir ())
val mlbpath = Path.new' here (hd (CommandLine.arguments ()))

;Benchmark.start ();
val basdecs = MLBParser.fromFile mlbpath
    handle MLBParser.Parse r => (Report.print r ; nil)
         | Path.Path r => (Report.print r ; nil)
;Benchmark.stop ();
;Benchmark.print "Parsing MLB file:";

(* ;Report.print (AstMLB.showBasdecs basdecs); *)

structure Map = OrderedMapFn (
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

(* Its much faster to just build a huge list of files and then calling fromList
   on that, than it is to incrementially build the set *)
fun parse ds =
    let
      open AstMLB Map

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
          | Source f =>
            if ignore f then
              ()
            else
              (* files := insert (!files) (explode (Path.path f)) *)
              (* files := insert (!files) f *)
              (case lookup (!files) f of
                 SOME ast => ()
               | NONE => files := update (!files) (f, SMLParser.fromFile f)
              )
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
      basdecs ds ; !files
    end

;Benchmark.start ();
val asts = parse basdecs
;Benchmark.stop ();
;Benchmark.print "Parsing source files:";

;Benchmark.start ();
val _ = Map.map (fn ast => Infixing.resolve (ast, Dictionary.empty)) asts
;Benchmark.stop ();
;Benchmark.print "Resolving infixes:";



