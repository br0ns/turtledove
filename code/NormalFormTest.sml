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

fun clauseMagic magic t =
    let open Tree Grammar
      fun loop nil e = (nil, e)
        | loop (p :: ps) e =
          let
            val (p', e) = magic (p, e)
            val (ps', e) = loop ps e
          in
            (p' :: ps', e)
          end
    in
      case this t of
        Clause v =>
        let
          val [ps, top, e] = children t
          val ps = children ps
          val (ps, e) = loop ps e
        in
          join (Clause v) [join Pats ps, top, clauseMagic magic e]
        end
      | n => join n (List.map (clauseMagic magic) $ children t)
    end

fun sortMatches t =
    let open Tree Grammar
      fun cmp t1 t2 =
          let
            fun pats t =
                case this t of
                  Clause _ => children $ hd $ children t
                | Rule => [hd $ children t]
                | _ => die "Illformed match"
          in
            List.collate
              (uncurry NormalForm.totalcmp)
              (pats t1, pats t2)
          end
    in
      case this t of
        Match => join Match $ List.sort cmp $ children t
      | n => join n (List.map sortMatches $ children t)
    end

fun extractFuns t =
    let open Tree Grammar
      fun extract cs =
          let
            val v =
                case this $ hd cs of
                  Clause v => v
            val cs =
                List.map
                  (fn c =>
                      case children c of
                        [pats, _, exp] =>
                        (children pats, exp)
                  )
                  cs
          in
            (v, cs)
          end
    in
      (case this t of
         Dec_Fun _ => List.map (extract o children) $ children t
       | _ => nil) @
      List.concatMap extractFuns (children t)
    end

fun elimLayers t = clauseMagic NormalForm.elimLayers t
fun elimWildcards t = clauseMagic NormalForm.elimWildcards t
fun elimUnit t = clauseMagic NormalForm.elimUnit t
fun gen t = clauseMagic NormalForm.gen t

fun var {environment, interface, infixing} name =
    let
      val id = Ident.fromString Fixity.Nonfix name
      val id =
          if Ident.isUnqual id then
            case Dictionary.lookup infixing name of
              SOME fixity => Ident.setFixity id fixity
            | NONE => id
          else
            id
      val vid = ValEnv.findVal environment id
      val var = Variable.ofIdent id
      val var = Variable.store var vid
    in
      var
    end

val cons = var bas "::"
val nill = var bas "nil"

fun isExhaustive (v, cs) =
    let
      val pss = List.map fst cs
    in
      List.all NormalForm.cover $ List.transpose pss
    end

val ast = elimLayers ast
val ast = elimWildcards ast
val ast = elimUnit ast
val ast = NormalForm.elimLists cons nill ast

val fs = extractFuns ast
val _ = List.app
          (fn f as (v, _) =>
              println (Variable.toString v ^ " is " ^
                       (if isExhaustive f then
                          ""
                        else
                          "not ") ^
                       "exhaustive")
          )
          fs

(* val ast = gen ast *)

;println "Before sorting:";
;Layout.println NONE $
                PPGrammar.showUnwrapped
                ast;
;println "After sorting:";
;Layout.println NONE $
                PPGrammar.showUnwrapped
                (sortMatches ast);
