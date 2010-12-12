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

fun walk sa sb t =
    let
      open Tree.Walk
      val print = Layout.println NONE
      fun loop w =
          let
            val _ = case Wrap.unwrap $ this w of
                      Dec_Include {file, ast, comments} =>
                      (println ("Reading " ^ Path.toString file) ;
                       loop (init ast) ;
                       println ("Leaving " ^ Path.toString file)
                      )
                    | _ => ()
            val _ = println "--"
            val _ = print $ show (SOME 2) false sa $ here w
            val _ = TextIO.print "> "
            val i = (hd o explode o valOf o TextIO.inputLine) TextIO.stdIn
          in
            case i of
              #"u" =>
              (case parent w of
                 SOME p => loop p
               | NONE   => ()
              )
            | #"1" =>
              (case children w of
                 c :: _ => loop c
               | _      => (println "No children." ;
                            loop w)
              )
            | #"2" =>
              (case children w of
                 _ :: c :: _ => loop c
               | _           => (println "No second child." ;
                                 loop w)
              )
            | #"3" =>
              (case children w of
                 _ :: _ :: c :: _ => loop c
               | _      => (println "No third child." ;
                            loop w)
              )
            | #"4" =>
              (case children w of
                 _ :: _ :: _ :: c :: _ => loop c
               | _           => (println "No fourth child." ;
                                 loop w)
              )
            | #"5" =>
              (case children w of
                 _ :: _ :: _ :: _ :: c :: _ => loop c
               | _      => (println "No fifth child." ;
                            loop w)
              )
            | #"6" =>
              (case children w of
                 _ :: _ :: _ :: _ :: _ :: c :: _ => loop c
               | _           => (println "No sixth child." ;
                                 loop w)
              )
            | #"s" => (print $ show NONE false sa $ here w ; loop w)
            | #"l" => (print $ sb $ Wrap.left $ this w ; loop w)
            | #"r" => (print $ sb $ Wrap.right $ this w ; loop w)
            | #"q" => ()
            | _    => (println "Press 'q', 'u', 's', 'l', 'r', or a number." ;
                       loop w)
          end
    in
      loop (init t)
    end

val _ = walk
          (fn {file, ast, comments} => Path.show file)
          (Layout.itemize "-" o List.map Path.show o Set.toList)
          project
