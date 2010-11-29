(* val print = fn s => print (s ^ "\n") *)

(* ;print "[Yeah baby!]"; *)

fun die e = (Layout.println (SOME 80) e ; OS.Process.exit OS.Process.failure)

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
val basdecs =
    let
      val {ast, ...} = MLBParser.fromFile mlbpath
    in
      ast
    end
    handle MLBParser.Parse r => die r
         | Path.Path r => die r
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
          case this t of
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
val srcs = sources basdecs
val dirs = Set.map Path.dir srcs

fun lexAndYacc dir =
    let
      val dirstream = OS.FileSys.openDir (Path.path dir)
      fun loop () =
          case OS.FileSys.readDir dirstream of
            SOME file =>
            if String.isSuffix ".lex" file orelse
               String.isSuffix ".yacc" file then
              Path.new (Path.path dir ^ "/" ^ file) :: loop ()
            else
              loop ()
          | NONE => nil
    in
      loop ()
    end

val files = Set.toList srcs @
            (List.concat o map lexAndYacc o Set.toList) dirs
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

val maxwidth = (size o
                Show.int o
                foldl (fn ((_, s), a) => Int.max (a, s)) 0
               ) files
fun spaces n = CharVector.tabulate (n, fn _ => #" ")

val r = Report.++ (
        Report.itemize
          (map (fn (f, s) =>
                   let
                     val ss = Show.int s
                   in
                     Report.text
                       (ss ^ " " ^
                        spaces (maxwidth - size ss) ^
                        Path.path' (Path.dir mlbpath) f)
                   end
               ) files
          ),
        Report.text (Int.toString (sz div 1000) ^
                     "KB in " ^
                     Int.toString (length files) ^ " files.")
        )

;Report.print r;


