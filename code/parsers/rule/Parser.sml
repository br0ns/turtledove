


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



val smlIdentifierParser = Lex.identifier {head = "", tail = ""}

val smlLongVidParser = Lex.lexeme $ many1 (smlIdentifierP --| (Text.char "."))

val smlVidParser = ...

val sconParser = Lex.lexeme $ Parser.Text.word


fun spatParser x = 
    let
      open Parser
      infix 0 |||
      infix 1 --- |-- --|
      infix 2 >>> --> ??? produce

      (*****************)
      (* atpat parsers *)
      (*****************)

      (* Wildcards in pattern "_"  *)
      val wildcardP = Lex.symbol "_" produce Wildcard                      

      (* record with zero or more key value pairs. *)
      val recordP = (Lex.braces $ Lex.commaSep recordKeyValP) >>> Record

      (* Record key value pairs: "id_l = id_r" or "id_l" or "..." =>
      "[(id_l,id_r), ..., (id_l, id_r)] where if only "id_l" is supplied it will
      expand to "(id_l, id_l)" and "..." will stop parsing for more input. *)
      val recordKeyValP = 
          ( 
           try smlIdentifierParser --> (fn idL =>           
           ( (* Either we are followed by a "=" and the new binding *)
               try $ (Lex.symbol "=") |-- 
               smlIdentifierP --> (fn idR => 
               return (idL, Longvid idR ))
           ) |||
           ( (* Or we bind the first id as the new binding as well *)
               return (idL, Longvid idL)
           ))           
          ) ||| 
          (
           lex.symbol "..." |--- fail 
          )

      (***************)
      (* pat parsers *)
      (***************)
          
      (* Constructed patterns such as SOME x, NONE or SOME 5 *)
      fun londVidAtPat x = (
          (smlLongVidParser --- atpatP)
      ) x
                       
      (***************************************)
      (* Resulting main parsers for patterns *)
      (***************************************)
                                                      
      and atpatP x = (
          wildcardP  
              ||| sconParser 
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

fun sexpparser x = 
    let
      open Parser
      infix 0 |||
      infix 1 --- |-- --|
      infix 2 >>> --> ??? produce

      (*****************)
      (* atexp parsers *)
      (*****************)

      (* Record with zero or more key value pairs.*)
      fun recordP x = (
          (Lex.braces $ Lex.commaSep recordKeyValP) >>> Record
      ) x              

      (* Record key value pairs: "id = exp" => (id, exp) *)
      and recordKeyValP x = (
          smlIdentifierParser --- (Lex.symbol "=" |-- expP)
      ) x

      (***************)
      (* exp parsers *)
      (***************)
                      
      (* Function application (left recursive): "exp atexp" => (exp, atexp)  *)
      and appP x = (
          (expP --- atexpP) >>> App
      ) x

      (* Anonymous function definition: "fn match" *)
      and fnP x = (
          (Lex.symbol "fn" |-- matchP) >>> Fun
      ) x

      (* Anonymous function matches: A rule followed by zero or more "| mrule"
         => [(pat,exp), ..., (pat_n, exp_n)] *)
      and matchP x = (
          sepBy1 mruleP (Lex.symbol "|")
      ) x 

      (* Anonymous function mrule: "pat => exp" => (pat, exp) *)
      and mruleP x = (
          spatParser --- (lex.symbol "=>" |-- sexpParser)
      ) x
         
      (******************************************)
      (* Resulting main parsers for expressions *)
      (******************************************)

        and atexpP x = (
            sconParser 
                ||| smlLongVidParser
                ||| recordP
                ||| Lex.parens expP
        ) x

        and expP x = (
            atexpP
                ||| appP
                ||| fnP
        ) x

    in

    end
