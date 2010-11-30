(* val print = fn s => print (s ^ "\n") *)

(* ;print "[Yeah baby!]"; *)

fun die e = (Layout.println (SOME 80) e ; OS.Process.exit OS.Process.failure)

val here = Path.new (OS.FileSys.getDir ())
val mlbpath = Path.new' here
              (hd (CommandLine.arguments ())
               handle _ => "Turtledove.mlb"
              )

val ast =
    let
      val {ast, ...} = MLBParser.fromFile mlbpath
    in
      ast
    end
    handle MLBParser.Parse r => die r
         | Path.Path r => die r

fun todos ast =
    let
      open MLBGrammar Set
      val this = Tree.this
      val children = Tree.children

      val visited = ref empty
      fun visit file = visited := insert (!visited) file
      fun ignore file =
          String.isSuffix ".lex.sml" (Path.path file) orelse
          String.isSuffix ".yacc.sig" (Path.path file) orelse
          String.isSuffix ".yacc.sml" (Path.path file) orelse
          file = Path.new "$(SML_LIB)/basis/basis.mlb" orelse
          file = Path.new "$(SML_LIB)/smlnj-lib/Util/smlnj-lib.mlb" orelse
          file = Path.new "$(SML_LIB)/mlyacc-lib/mlyacc-lib.mlb" orelse
          not (Path.sub (Path.dir mlbpath) file) orelse
          member (!visited) file

      fun totodo s =
          let
            open Scanner
            infix 0 |||
            infix 1 --- |-- --|
            infix 2 >>> --> ??? produce underlies
            fun empty s =
                List.all Char.isSpace $ explode s

            val spaces = many $ Text.char #" "
            val graph = predicate Char.isGraph
            val first =
                spaces
                  |-- Text.string "TODO"
                  |-- many graph
                  |-- spaces
                  |-- Text.line
            val prefix =
                spaces
                  |-- maybe (Symb.asterisk --- spaces)
            val rest =
                prefix
                  |-- Text.line
            val last =
                prefix
                  |-- many any >>> implode
            val p = first --- many rest --- last
          in
            Option.map
              (fn ((f, r), l) =>
                  (if empty f then
                     nil
                   else
                     [f]) @
                  r @
                  (if empty l then
                     nil
                   else
                     [l])
              )
              (Parse.string p s)
          end

      fun loop t =
          case this t of
            Dec_Source file =>
            if ignore file then
              nil
            else
              let
                open Layout SourceText
                infix ^^ ++ \ & \\ &&

                val {comments, ...} = SMLParser.fromFile file
                val ts =
                    List.mapPartial
                    (fn (p, c) =>
                        Option.map
                          (fn ls =>
                              (txt $ posToString (fromFile file) p)
                                       \ (indent 2 $ vsep $ List.map txt ls)
                          )
                          (totodo c)
                    ) comments
              in
                visit file ;
                ts
              end
          | Dec_Include {file, ast, ...} =>
            if ignore file then
              nil
            else
              loop ast
          | _ => List.concatMap loop $ children t
    in
      loop ast
    end

val ts = todos ast
val _ =
    let open Layout in
      println NONE $ vsep $ punctuate ln $ ts
    end
