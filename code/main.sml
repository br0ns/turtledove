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
(* ;Benchmark.print (); *)

(* ;Report.print (AstMLB.showBasdecs basdecs); *)

(* Den ordnede mængde er ulideligt langsom. Der må være en fejl et sted *)

(* structure Set = OrderedSetFn (struct *)
(*                               type t = File.t *)
(*                               fun compare p p' = String.compare (Path.toString p, Path.toString p') *)
(*                               val toString = Path.toString *)
(*                               end) *)
fun sources ds =
    let
      open AstMLB
      open Set

      fun ignore file =
          List.exists (fn f => f = Path.file file)
                      ["MLB.lex.sml", "MLB.yacc.sig", "MLB.yacc.sml"]
          orelse file = Path.new "$(SML_LIB)/basis/basis.mlb"
          orelse file = Path.new "$(SML_LIB)/smlnj-lib/Util/smlnj-lib.mlb"
          orelse file = Path.new "$(SML_LIB)/mlyacc-lib/mlyacc-lib.mlb"

      val depth = ref 0
      fun basdecs ds = List.foldl (fn (s, a) => union s a) empty (List.map basdec ds)
      and basdec d =
          case d of
            Basis bs => basbinds bs
          | Local (ds, ds') => union (basdecs ds) (basdecs ds')
          | Include (f, ds) => if ignore f then
                                 empty
                               else
                                 basdecs ds
          | Source f => if ignore f then
                          empty
                        else
                          singleton f
          | Ann (_, ds) => basdecs ds
          | _ => empty
      and basexp e =
          case e of
            Bas ds => basdecs ds
          | Let (ds, e) => union (basdecs ds) (basexp e)
          | Var _ => empty
      and basbinds bs = List.foldl (fn (s, a) => union s a) empty (List.map basbind bs)
      and basbind (_, e) = basexp e
    in
      if Path.file mlbpath = "Turtledove.mlb" then
        union
          (fromList [Path.new' here "parsers/MLB.lex",
                     Path.new' here "parsers/MLB.yacc"])
          (basdecs ds)
      else
        basdecs ds
    end

;Benchmark.start ();
val srcs = sources basdecs
;Benchmark.stop ();
(* ;Benchmark.print (); *)

val srcs = map (fn f => (f, File.size f))
               (Set.toList srcs)
val srcs = ListSort.sort (fn (_, x) => fn (_, y) => Int.compare (x, y)) srcs

val sz = foldl (fn ((_, s), a) => a + s) 0 srcs

val r = Report.++ (
        Report.itemize
            (map (fn (f, s) =>
                     Report.text (Int.toString s ^ " " ^ Path.path' here f)
                 ) srcs
            ),
        Report.text (Int.toString (sz div 1000) ^ "KB in " ^ Int.toString (length srcs) ^ " files.")
        )

;Report.print r;
