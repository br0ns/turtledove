open MLBGrammar

fun die e = (Layout.println (SOME 80) e ; OS.Process.exit OS.Process.failure)

val here = Path.new (OS.FileSys.getDir ())

val [mlb, sml] = List.map (Path.new' here) $ CommandLine.arguments ()

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


val SOME bas = basis project $ Path.new "$(SML_LIB)/basis/basis.mlb"
val SOME ast = find project sml
val ast = NormalForm.unwrap ast
val ast' = Magic.dust bas ast

local open Layout infix \ in
val unsorted = txt "Before:" \ PPGrammar.showUnwrapped ast
val sorted = txt "After:" \ PPGrammar.showUnwrapped ast'
val _ = Layout.println NONE $ besides 4 (unsorted, sorted)
end

