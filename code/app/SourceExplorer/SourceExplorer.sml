open MLBGrammar

fun die e = (Layout.println (SOME 80) e ; OS.Process.exit OS.Process.failure)

val here = Path.new (OS.FileSys.getDir ())
val path =
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


(* fun show ast = Layout.println NONE $ Grammar.show ast *)

(* val {ast, ...} = SMLParser.fromFile path *)
(* ;show ast; *)
(* val {ast, ...} = Resolve.infixing ast *)
(* ;show ast; *)

;Benchmark.start ();
val {ast = project, ...} = Main.init path
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

(* ;Benchmark.start (); *)
(* val mlb = map (fn f => ( *)
(*                   (\* Benchmark.start () ; *\) *)
(*                   case lookup f of *)
(*                     SOME x => x *)
(*                   | NONE => *)
(*                     let *)
(*                       val {ast, comments} = SMLParser.fromFile f *)
(*                     in *)
(*                       insert (f, {file = f, ast = ast, comments = comments}) *)
(*                     end *)
(*                   (\* Benchmark.stopAndPrint ("Parsed " ^ Path.toString f) *\) *)
(*                   ) *)
(*                   handle e => raise Fail (exnName e ^ " in " ^ Path.toString f) *)
(*               ) *)
(*               mlb *)
(*               handle SMLParser.Parse s => die s *)
(* ;Benchmark.stopAndPrint ""; *)

fun walk walk' sa sb t =
    let
      open Tree.Walk
      val print = Layout.println NONE
      exception Quit
      fun loop w =
          let
            val _ = println "--"
            val _ = print $ show (SOME 2) false sa $ here w
            val _ = TextIO.print "> "
            val i = valOf $ TextIO.inputLine TextIO.stdIn
            fun read walk file ast =
                (println ("Reading " ^ Path.toString file) ;
                 walk file ast handle Quit => () ;
                 println ("Leaving " ^ Path.toString file)
                )
          in
            (if i = "\n" then
               case Wrap.unwrap $ this w of
                 Dec_Include {file, ast, comments, basis} =>
                 read (fn _ => loop o init) file ast
               | Dec_Source {file, ast, comments} =>
                 read walk' file ast
               | _ => ()
             else
               case String.sub (i, 0) of
                 #"u" =>
                 (case parent w of
                    SOME p => loop p
                  | NONE   => raise Quit
                 )
               | #"s" => print $ show NONE false sa $ here w
               | #"l" => print $ sb $ Wrap.left $ this w
               | #"r" => print $ sb $ Wrap.right $ this w
               | #"q" => raise Quit
               | _ =>
                 case Int.fromString i of
                   NONE => ()
                 | SOME n =>
                   loop $ List.nth (children w, n - 1)
                   handle Subscript => ()
            ) ; loop w
          end
    in
      loop (init t) handle Quit => TextIO.println "Goodbye"
    end

fun walk' sb file t =
    let
      open Tree.Walk
      val print = Layout.println NONE
      val st = SourceText.fromFile file
      exception Quit
      fun loop w =
          let
            val _ = println "--"
            val showid = Ident.toString o Wrap.unwrap
            val showvar = Variable.toString o Wrap.unwrap
            val _ = print $ Grammar.showWrapped (SOME 2) $ here w
            val _ = TextIO.print "> "
            val i = valOf $ TextIO.inputLine TextIO.stdIn
            fun varToString v =
                let
                  val v = Wrap.unwrap v
                  val id = Ident.toString $ Variable.ident v
                  val vid = ValEnv.vidToString $ Variable.load v
                in
                  id ^ ": " ^ vid
                end
          in
            (case String.sub (i, 0) of
               #"u" =>
               (case parent w of
                  SOME p => loop p
                | NONE   => raise Quit
               )
             | #"s" => TextIO.println
                         $ SourceText.getSource
                         st
                         (#position $ Wrap.left $ this w)
                         (#position $ Wrap.right $ this w)
             | #"i" =>
               let open Grammar in
                 TextIO.println
                   (case Wrap.unwrap $ this w of
                      Constructor v =>
                      varToString v
                    | Replication (v1, v2) =>
                      varToString v1 ^ " = " ^ varToString v2
                    | Dec_Overload (_, v, _) =>
                      varToString v
                    | Clause v =>
                      varToString v
                    | Exp_Var v =>
                      varToString v
                    | Label_Short v =>
                      varToString v
                    | Pat_Var v =>
                      varToString v
                    | _ => raise Quit
                   ) handle Quit => ()
               end
             | #"l" => print $ sb $ Wrap.left $ this w
             | #"r" => print $ sb $ Wrap.right $ this w
             | #"q" => raise Quit
             | _ =>
               case Int.fromString i of
                 NONE => ()
               | SOME n =>
                 loop $ List.nth (children w, n - 1)
                 handle Subscript => ()
            ) ; loop w
          end
    in
      loop (init t) handle Quit => ()
    end


val _ = walk
          (walk'
             (* (const $ Layout.txt "Din MOR") *)
             (fn {environment, position} => ValEnv.show environment)
          )
          (fn {file, ast, comments} => Path.show file)
          (Layout.itemize "-" o List.map Path.show o Set.toList)
          project
          handle e => println (exnName e ^ ": " ^ exnMessage e)
