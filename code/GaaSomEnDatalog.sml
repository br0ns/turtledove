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
                        " file") ;
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

fun walk' sa sb file t =
    let
      open Tree.Walk
      val print = Layout.println NONE
      val st = SourceText.fromFile file
      exception Quit
      fun loop w =
          let
            val _ = println "--"
            val _ = print $ Grammar.show (SOME 2) sa $ here w
            val _ = TextIO.print "> "
            val i = valOf $ TextIO.inputLine TextIO.stdIn
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
                         (Wrap.left $ this w)
                         (Wrap.right $ this w)
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
             (fn id =>
                 let
                   val id = Wrap.unwrap id
                   fun iopt (SOME n) = Int.toString n
                     | iopt NONE = ""
                 in
                   (case Ident.fixity id of
                      Fixity.InfixL n => "(L" ^ iopt n ^ ") "
                    | Fixity.InfixR n => "(R" ^ iopt n ^ ") "
                    | Fixity.Nonfix => ""
                    | Fixity.Op => "(op)"
                   ) ^ Ident.toString id
                 end
             )
             (Layout.int)
          )
          (fn {file, ast, comments} => Path.show file)
          (Layout.itemize "-" o List.map Path.show o Set.toList)
          project
