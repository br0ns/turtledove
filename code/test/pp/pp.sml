open MLBGrammar

fun die s = Crash.impossible $ "pp: " ^ s

val testPath = "files/"
val testMLB = "test.mlb"
val testSML = "test.sml"

val here = Path.new (OS.FileSys.getDir ())
val pathMLB = Path.new' here (testPath ^ testMLB)
val pathSML = Path.new' here (testPath ^ testSML)


val _ = if File.exists pathMLB then
          println $ "The test MLB file exists."
        else
          die $ "The test MLB file does not exists. Create one in: "
                ^ Path.toString pathMLB

val _ = if File.exists pathSML  then
          println $ "The test sml file exists."
        else
          die $ "The test sml file does not exists. Create one in: "
                ^ Path.toString pathSML

val {ast = project, ...} = Main.init pathMLB
    handle Main.Error e => die $ Layout.pretty NONE e
         | e => (println (exnName e ^ ": " ^ exnMessage e)
               ; raise Fail "foo"
                )
val project = Resolve.init project
    handle e =>
           (println (exnName e ^ ": " ^ exnMessage e)
          ; raise Fail "foo"
           )

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


val ast = case find project pathSML of
            SOME ast => ast
          | _ => die $  "Could not find the file: " 
                        ^ Path.file pathSML ^ "in the MLB file: " 
                        ^ Path.file pathMLB

(*
val _ = Tree.this ast : int
val _ = (Wrap.unwrap $ Tree.this ast) : int
val _ = PPGrammar Ident.show Variable.show 8
*)

val _ = TextIO.println "Wrapped AST:"
val _ = Layout.println NONE $
        Grammar.showWrapped NONE ast

val _ = TextIO.println "Wrapped PP:"
val _ = Layout.println NONE $ PPGrammar.showWrapped ast

val ast_unwrapped = NormalForm.unwrap ast

val _ = TextIO.println "UnWrapped AST:"
val _ = Layout.println NONE $
        Grammar.showUnwrapped NONE ast_unwrapped

val _ = TextIO.println "UnWrapped PP:"
val _ = Layout.println NONE $ PPGrammar.showUnwrapped ast_unwrapped



(*
        test ()
      (* Debug print the current part of the ast, depth = 0*)
      fun pp t = (Layout.println NONE $ Grammar.show Grammar.isWrapped
                                                     (SOME 1) t)

      (* Debug print the current part of the ast, depth = 0*)
      fun pp t = (Layout.println NONE $ Grammar.show (Ident.toString o Wrap.unwrap)
                                                     (Variable.toString o Wrap.unwrap)
                                                     (SOME 1) t)
*)
