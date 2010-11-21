(* TODO: empty productions mess with the position information. Fix that. *)
structure Parser =
struct

structure RuleLrVals = RuleLrValsFun
                        (structure Token = LrParser.Token)

structure RuleLex    = RuleLexFun
                        (structure Tokens = RuleLrVals.Tokens)



fun lex str = 
    let
      val st = SourceText.fromString str
      val source = Source.fromSourceText st
      val reader = Source.makeReader source
    in
      RuleLex.makeLexer reader source
    end


structure T = LrParser.Token
structure PD = RuleLrVals.ParserData
structure EC = RuleLrVals.ParserData.EC
structure MV = RuleLrVals.ParserData.MlyValue

(* To be placed after the svalue datatype
fun showSV (META f) = f ()
  | showSV (TRANS f) = f ()
  | showSV (CHAR f) = f ()
  | showSV (INT f) = f ()
  | showSV (LONGID f) = f ()
  | showSV (REAL f) = f ()
  | showSV (STRING f) = f ()
  | showSV (TYVAR f) = f ()
  | showSV (WORD f) = f ()
  | showSV _ = ""
end
*)

fun showToken (T.TOKEN (trm, (sv, p1, p2))) = 
    print $ (EC.showTerminal trm)
    ^ " '" ^(MV.showSV sv) ^ "'"
    ^ "\t\t (Start: " ^ (Int.toString p1)
    ^ " End: " ^ (Int.toString p2)
    ^ ")\n";

fun showTokens str =
    let
      val l = lex str
      fun loop () =
          let
            val tk as (T.TOKEN (trm, _)) = l ()
          in
            (showToken tk;
             case (EC.showTerminal trm) of
               "EOF" => ()
             | sTrm => loop()
            )
          end
    in
      loop ()
    end

fun readFile f = 
    let 
      val is = TextIO.openIn f
    in
      TextIO.inputAll is before TextIO.closeIn is
    end


(*
fun t c = (print $ Int.toString $ ord c; print "\n")

val _ = List.app t $ explode $ readFile "test/test.rule"
*)

(* val _ = showTokens $ readFile "test/test.rule" *)

val _ = showTokens $ readFile "test/fold.rule" 


(*

structure SMLParser = JoinWithArg
                        (structure ParserData = SMLLrVals.ParserData
                         structure Lex = SMLLex
                         structure LrParser = LrParser)
*)

exception Parse of Report.t
  
(*
fun fromSourceText st =
    let
      val file = SourceText.getFile st
      fun context r = Report.++ (r, Report.text ("while parsing " ^ Path.path file))
      fun fail r = raise Parse (context r)
    in
      let
        val source = Source.fromSourceText st
        val reader = Source.makeReader source

        (* The parser parses until EOF so the rest of the stream is always empty *)
        val ((topdecs, comments), _) =
            SMLParser.parse (Constants.PARSE_LOOKAHEAD,
                             SMLParser.makeLexer reader source,
                          fn (s, l, r) => Source.error source l s,
                             source)
      in
        {topdecs = topdecs, comments = comments}
      end handle LexError r => fail r
               | Path.Path r => fail r
               | IO.Io {name, cause = OS.SysErr (err, se), ...} =>
                 fail (Report.text (
                       (case se of
                          NONE    => err
                        | SOME m  => OS.errorMsg m
                       ) ^ ": " ^ name)
                      )
               | IO.Io {name, ...} =>
                 fail (Report.text ("Failed to read file: " ^ name))
    end

val fromFile = fromSourceText o SourceText.fromFile
*)
end
