


datatype ruleType = Clauses
                  | Expression

type ruleName = string

type ruleHeader = ruleType * ruleName

datatype scon = Str of string (* FIX *)


(*
(* pat and atpat is merged for simplicity of representation *)
datatype pat = PatWildcard
             | PatScon of scon                   (* Special constant *)
             | PatLongvid of string              (* Value indentier *)
             | PatLongvidAtPat of string * pat   (* Constructed pattern *)
             | PatRecord of (string * pat) list  (* pat fix StringMap.t *)
             | PatMetaPat of metaPattern
withtype metaPattern = string * pat (* Uppercase name, input *)


(* exp and atexp is merged for simplicity of representation *)
datatype exp = ExpScon of scon                  (* Special constant *)
             | Longvid of string             (* value identifier *)
             | Record of (string * exp) list (* MAKE TO A MAP *)
             | App of exp * exp              (* Function application, left recursive *)
             | Fun of mrule list             (* anonymous function definition *)
             | Trans of transformer
             | MetaPat of metaPattern
withtype transformer = string * pat          (* Uppercase name, input *)
     and mrule = pat * exp                   (* pat => exp *)
*)

(* pat, atpat and exp, atexpis has merged for simplicity of representation *)
datatype patexp = Wildcard
                | Scon of scon                      (* Special constant *)
                | Longvid of string list            (* Value indentier - each string in the list
                                                       was delimited by a period. *)
                | LongvidAtPat of string list * patexp (* Constructed pattern - each string in the 
                                                          list was delimited by a period. *)
                | Layered of string * patexp        (* Layered vid * pat *)
                | Record of (string * patexp) list  (* pat fix StringMap.t *)
                | App of exp * exp                  (* Function application, left recursive *)
                | Fun of mrule list                 (* anonymous function definition *)
                | Trans of transformer
                | MetaPat of metaPattern
withtype transformer = string * pat          (* Uppercase name, input *)
     and metaPattern = string * patexp (* Uppercase name, input *)
     and mrule = pat * exp                   (* pat => exp *)

type sexp = patexp
type spat = patexp

datatype clause = Clause of spat * sexp

type cstrn = string (* needs fixing *)

datatype scheme = Scheme of clause list * cstrn list

datatype rule = Rule of ruleHeader * scheme * clause list



val smlIdentifierP = Lex.identifier {identHd = "", identTl = ""}

val smlLongVidP = Lex.lexeme $ many1 (smlIdentifierP --| (Text.char "."))

val smlVidP = ...

val sconP = Lex.lexeme $ Parser.Text.word


fun spatParser x = 
    let
      open Parser
      infix 0 |||
      infix 1 --- |-- --|
      infix 2 >>> --> ??? produce

      (* atpat parsers *)

      val wildcardP = Lex.symbol "_" >>> Wildcard                      

      val recordKeyValP = 
          ( 
           smlIdentifierP 
               --> (fn id1 =>                        
                       ( (* Either we are followed by a "=" and the new binding *)
                        (Lex.symbol "=")
                            |-- smlIdentifierP
                            --> (fn id2 => return (id1, Longvid id2 ))
                       ) |||
                       ( (* Or we bind the first id as the new binding as well *)
                        return (id1, Longvid id1)
                       )
                   )
          ) (*Fixme: We ought to parse the "..." as well *)

      val recordP = (Lex.braces $ Lex.commaSep recordKeyValP) >>> Record


      (* pat parsers *)

      fun layeredP x = (
          smlVidP
              --> (fn vid => 
                      (Lex.symbol "as")
                          |-- patP >>> (pair vid)
                  )
      ) x 
                       
      and londVidAtPat x = (
          smlLongVidP --> (fn lngVid => atpatP >>> (pair lngVid) )
      ) x
                           
                           
                           
      and atpatP x = (
          wildcardP  
              ||| sconP 
              ||| smlLongVidP 
              ||| recordP 
              ||| Lex.parens patP                   
      ) x
      and patP = (
          atpatP 
              ||| longVidAtpat
              ||| layeredP
      ) x
                   
    in
      patp x
    end

fun parser x =
    let
      open Parser
      infix 0 |||
      infix 1 --- |-- --|
      infix 2 >>> --> ??? produce

      val idP =  (Lex.lexeme $ many1 Text.letter) >>> (Nullary o implode)
      val conP = ... Unary
      val tupleP = (Lex.parens $ Lex.commaSep1 patP) >>> Tuple

      (* Record parser, parses sml records with zero or more recordKeyVal
      pairs.*)
      fun recordP x = (
          (Lex.braces $ commaSep recordKeyValP) >>> Record
      ) x

      (* record key value parser, parses an sml identifier folowed by a "=" and
      then a pattern *)
      and recordKeyValP x = (
          smlIdentifierP --> (fn id => symbol "=" |-- patP >>> (pair id))
      ) x
                            
      (* Meta pattern parser for the metaPattern type *)
      and metaP x = (
          
          ) x

      (* Transformer parser for the trans datatype *)
      and transP x = (
          symbol "£" |-- (Lex.Lexeme $ many1 Text.upper) --> 
                 (fn transName => patP >>> (pair transName))
          ) x                  

      (* Pattern parser for the pat datatype. *) 
      and patP x = (
          idP ||| tupleP
      ) x
                       
    in
      Text.whitespace |-- patP --> (fn p => eof >>> p)
    end
*)
