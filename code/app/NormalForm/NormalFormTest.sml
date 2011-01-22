open MLBGrammar

fun die e = (Layout.println (SOME 80) e ; OS.Process.exit OS.Process.failure)
fun quit e = (Layout.println (SOME 80) e ; OS.Process.exit OS.Process.success)

val here = Path.new (OS.FileSys.getDir ())


val (flags, mlbFiles, smlFiles) = 
    let
      val args = CommandLine.arguments ()
      val _ = length args < 2 andalso die $ Layout.txt "Needs atleast two arguments, mlb-file and sml-file."
      val (flags, files) = List.partition (fn s => String.isPrefix "--" s) args
      val (mlbFiles, smlFiles) = List.partition 
                                     (fn s => String.extract(s, size s - 3, NONE) = "mlb") 
                                     files
      val mlbFiles' = List.map (Path.new' here) mlbFiles
      val smlFiles' = List.map (Path.new' here) smlFiles
    in
      (flags, mlbFiles', smlFiles')
    end

val _ = List.exists (fn s => s = "--help") flags andalso
          quit $ Layout.txt $ "You are on your own..\n\n"
               ^ "Btw there is one flag:\n"
               ^ "--no-column"

val mlb = case mlbFiles of
            [mlb] => mlb
          | _ => die $ Layout.txt "Only one mlb file should be given"

val sml = case smlFiles of
            [sml] => sml
          | _ => die $ Layout.txt "Only one sml file should be given"
             

val _ = print $ "Normalising: " ^ Path.file sml ^ "\n"

(* fun show ast = Layout.println NONE $ Grammar.show ast *)

(* val {ast, ...} = SMLParser.fromFile path *)
(* ;show ast; *)
(* val {ast, ...} = Resolve.infixing ast *)
(* ;show ast; *)

;Benchmark.start ();
val {ast = project, ...} = Main.init mlb
    handle Main.Error e => die e
         | e => (println (exnName e ^ ": " ^ exnMessage e)
               ; raise Fail "foo"
                )

;Benchmark.stopAndPrint "";

;Benchmark.start ();
val project = Resolve.init project
    handle e =>
           (println (exnName e ^ ": " ^ exnMessage e)
          ; raise Fail "foo"
           )
;Benchmark.stopAndPrint "";

structure Map = Path.Map
structure Set = Path.Set

fun find t target =
    let open Tree Wrap MLBGrammar
      fun loop nil = NONE
        | loop (t :: ts) =
          case find t target of
            SOME ast => SOME ast
          | NONE => loop ts
    in
      case unwrap $ this t of
        Dec_Source {file, ast, comments} =>
        if file = target then
          SOME ast
        else
          NONE
      | _ => loop $ children t
    end

fun basis t target =
    let open Tree Wrap MLBGrammar
      fun loop nil = NONE
        | loop (t :: ts) =
          case basis t target of
            SOME bas => SOME bas
          | NONE => loop ts
    in
      case unwrap $ this t of
        Dec_Include {file,
                     ast,
                     comments,
                     basis} =>
        if file = target then
          SOME basis
        else
          NONE
      | _ => loop $ children t
    end


val bas = case basis project $ Path.new "$(SML_LIB)/basis/basis.mlb" of
            SOME bas => bas
          | _ => die $ Layout.txt $ "Could not find the basis declaration "
                 ^ "\"$(SML_LIB)/basis/basis.mlb\" in the mlb file"

val ast = case find project sml of
            SOME ast => NormalForm.unwrap ast
          | _ => die $ Layout.txt $ "Could not find the file (" 
                                    ^ Path.file sml 
                                    ^ ") to normalise in the mlb file"

fun isExhaustive (v, cs) =
    let
      val pss = List.map fst cs
    in
      NormalForm.cover pss
    end

val ast' = NormalForm.normalize bas ast

local open Layout infix \ ^^ in

val unsorted = txt "Before:" \ PPGrammar.showUnwrapped ast
val sorted = txt "After:" \ PPGrammar.showUnwrapped ast'
val _ = if List.exists (fn s => "--no-column" = s) flags then
          Layout.println NONE $ (unsorted ^^ brk ^^ brk ^^ sorted)
        else
          Layout.println NONE $ besides 4 (unsorted, sorted)
end