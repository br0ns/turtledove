val print = fn s => print (s ^ "\n")

;print "[Yeah baby!]";

val here = Path.new (OS.FileSys.getDir ())
val mlbpath = Path.new' here "Turtledove.mlb"

val basdecs = MLBParser.fromFile mlbpath
    handle MLBParser.Parse r => (Report.print r ; nil)
         | Path.Path r => (Report.print r ; nil)

fun todos ds =
    let
      open AstMLB

      val visited = ref Set.empty

      fun ignore file =
          String.isSuffix "MLB.lex.sml" (Path.path file) orelse
          String.isSuffix "MLB.yacc.sig" (Path.path file) orelse
          String.isSuffix "MLB.yacc.sml" (Path.path file) orelse
          String.isSuffix "SML.lex.sml" (Path.path file) orelse
          String.isSuffix "SML.yacc.sig" (Path.path file) orelse
          String.isSuffix "SML.yacc.sml" (Path.path file) orelse
          file = Path.new "$(SML_LIB)/basis/basis.mlb" orelse
          file = Path.new "$(SML_LIB)/smlnj-lib/Util/smlnj-lib.mlb" orelse
          file = Path.new "$(SML_LIB)/mlyacc-lib/mlyacc-lib.mlb" orelse
          not (Path.sub (Path.dir mlbpath) file)

      fun basdecs ds = List.app basdec ds
      and basdec d =
          case d of
            Basis bs => basbinds bs
          | Local (ds, ds') => (basdecs ds ; basdecs ds')
          | Include (f, ds) => if ignore f then
                                 ()
                               else
                                 basdecs ds
          | Source f =>
            if ignore f orelse Set.member (!visited) f then
              ()
            else
              (* files := insert (!files) (explode (Path.path f)) *)
              (* files := insert (!files) f *)
              let
                val _ = visited := Set.insert (!visited) f
                open Report infix ++
                val {comments, ...} = SMLParser.fromFile f
                fun isTodo s =
                    let
                      fun doit (#"T" :: #"O" :: #"D" :: #"O" :: _) = true
                        | doit (c :: cs) = Char.isSpace c andalso doit cs
                        | doit _ = false
                    in
                      doit (explode s)
                    end
                fun spaces 0 = ""
                  | spaces n = " " ^ spaces (n - 1)
                val comments = List.filter (fn (_, c) => isTodo c) comments
                fun ctor (p, c) =
                    let
                      open SourceText
                      val {column, ...} = posToRowCol (fromFile f) p
                    in
                      text (spaces column ^ c)
                    end
              in
                case comments of
                  nil => ()
                | _ => Report.print (
                       text (Path.path' here f) ++
                            (indent o column o map ctor)
                            comments
                       )
                (* | _ => print "bleh" *)
              end
          | Ann (_, ds) => basdecs ds
          | _ => ()
      and basexp e =
          case e of
            Bas ds => basdecs ds
          | Let (ds, e) => (basdecs ds ; basexp e)
          | Var _ => ()
      and basbinds bs = List.app basbind bs
      and basbind (_, e) = basexp e
    in
      basdecs ds
    end

val _ = todos basdecs



