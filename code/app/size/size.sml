(* val print = fn s => print (s ^ "\n") *)

(* ;print "[Yeah baby!]"; *)

fun die e = (println e ; OS.Process.exit OS.Process.failure)

datatype orderby = Size
                 | Name

val here = Path.new (OS.FileSys.getDir ())
val mlbpath =
    Path.new'
      here
      (hd (CommandLine.arguments ())
       handle _ =>
              (println ("Usage: " ^
                        CommandLine.name () ^
                        " file.mlb") ;
               OS.Process.exit OS.Process.success
              )
      )

val orderby =
    (if List.nth (CommandLine.arguments (), 1) = "name" then
       Name
     else
       Size) handle Subscript => Size

(* ;Benchmark.start (); *)
val ast =
    let
      val {ast, ...} = MLBParser.fromFile mlbpath
    in
      ast
    end
    handle MLBParser.Error (_, e) => die e
         | MLBParser.LexError (_, e) => die e
         | MLBParser.YaccError (_, e) => die e
         | Path.Path e => die e
(* ;Benchmark.stop (); *)
(* ;Benchmark.print "Parsing MLB file:"; *)

structure Set = OrderedSetFn (
                struct
                type t = Path.t
                fun compare x y = String.compare (Path.path x, Path.path y)
                val toString = Path.toString
                end)

fun sources ast =
    let
      open MLBGrammar Set
      val this = Tree.this
      val children = Tree.children

      val files = ref empty
      fun ignore file =
          String.isSuffix ".lex.sml" (Path.path file) orelse
          String.isSuffix ".yacc.sig" (Path.path file) orelse
          String.isSuffix ".yacc.sml" (Path.path file) orelse
          file = Path.new "$(SML_LIB)/basis/basis.mlb" orelse
          file = Path.new "$(SML_LIB)/smlnj-lib/Util/smlnj-lib.mlb" orelse
          file = Path.new "$(SML_LIB)/mlyacc-lib/mlyacc-lib.mlb" orelse
          not (Path.sub (Path.dir mlbpath) file)

      fun loop t =
          case Wrap.unwrap $ this t of
            Dec_Source file =>
            if ignore file then
              ()
            else
              files := insert (!files) file
          | Dec_Include {file, ast, ...} =>
            if ignore file then
              ()
            else
              loop ast
          | _ => List.app loop $ children t
    in
      loop ast ; !files
    end

(* ;Benchmark.start (); *)
val srcs = sources ast
val dirs = Set.map Path.dir srcs

fun extra p dir =
    let
      val dirstream = OS.FileSys.openDir (Path.path dir)
      fun loop () =
          case OS.FileSys.readDir dirstream of
            SOME file =>
            if p file then
              Path.new (Path.path dir ^ "/" ^ file) :: loop ()
            else
              loop ()
          | NONE => nil
    in
      loop ()
    end

val lexAndYacc =
    extra
      (fn file =>
          String.isSuffix ".lex" file orelse
          String.isSuffix ".yacc" file
      )

val makefile =
    extra
      (fn file => file = "Makefile")

val files = Set.toList srcs @
            (List.concat o map lexAndYacc o Set.toList) dirs @
            (List.concat o map makefile o Set.toList) dirs
(* ;Benchmark.stop (); *)
(* ;Benchmark.print "Collecting source files:"; *)

val files = map (fn f => (f, File.size f)) files

val compare =
    case orderby of
      Size => (fn (_, x) => fn (_, y) => Int.compare (x, y))
    | Name => (fn (x, _) => fn (y, _) => String.compare (Path.path x,
                                                         Path.path y))
val files = List.sort compare files

val sz = foldl (fn ((_, s), a) => a + s) 0 files

val _ =
    let
      open Layout infix ^^ \ \\ & && ++
      val (fs, ss) = ListPair.unzip files
      val path = txt o Path.path' (Path.dir mlbpath)
      fun size s = int s ^^ txt "B"
      val ls =
          besides
            2
            (vsep $ List.map path fs,
             flushRight $ vsep $ List.map size ss)
      val s = indent 2 ls \
                     txt "Total"
                     ++ size sz
                     ++ txt "in"
                     ++ int (List.length files)
                     ++ txt "files."
    in
      println (SOME 120) s
      (* println NONE s *)
    end


