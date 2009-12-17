open MLBGrammar
open MLBGrammarUtils

%%

%name MLB

%term
      ID of string | COMMA | SEMICOLON | EOF
    | AND | BAS | BASIS | END | EQUALOP | FUNCTOR | IN | LET 
    | LOCAL | OPEN | SIGNATURE | STRUCTURE
    | ANN | PRIM | FILE of string | STRING of string

%nonterm
         ann of string
       | annPlus of string list
       | annStar of string list
       | basbinds of basbinds
       | basbinds' of Basexp.t * basbinds
       | basbinds'' of basbinds
       | basdec of Basdec.t
       | basdecnode of Basdec.node
       | basdecs of Basdec.t
       | basdecsnode of Basdec.node 
       | basexp of Basexp.t
       | basexpnode of Basexp.node
       | basid of Basid.t
       | basids of Basid.t list
       | fctbinds of fctbinds
       | fctbinds' of Fctid.t * fctbinds
       | fctbinds'' of fctbinds
       | fctid of Fctid.t
       | id of Symbol.t * Region.t
       | mlb of Basdec.t
       | sigbinds of sigbinds
       | sigbinds' of Sigid.t * sigbinds
       | sigbinds'' of sigbinds
       | sigid of Sigid.t
       | strbinds of strbinds
       | strbinds' of Strid.t * strbinds
       | strbinds'' of strbinds
       | strid of Strid.t

%pos int
%arg (st) : SourceText.source_text
%eop EOF
%noshift EOF
%verbose

%keyword LOCAL IN END BASIS OPEN BAS LET

%start Start

%%

Start:
             BDec
                ( Bdec )

BDec:
             BDec BDec
                ( Seq_bdec (BDec1, BDec2) )
           | (* epsilon *)
                ( Empty_bdec )
           | LOCAL BDec IN BDec END
                ( Local_bdec (BDec1, BDec2) )
           | BASIS ID EQ BExp
                ( Basis_bdec (mkId ID, BExp) )
           | OPEN LongBId_list
                ( Open_bdec LongBId_list )
           | ID
                ( if isMLB ID then
                      Include_bdec ID
                  else
                      Source_bdec ID )

BExp:
             BAS BDec END
                ( Dec_bexp BDec )
           | LET BDec IN BExp END
                ( Let_bexp (BDec, BExp) )
           | ID
                ( mkLongId ID )

LongBId_list:
             ID LongBId_list
                ( mkLongId ID :: LongBId_list )
            | (* epsilon *)
                ( nil )
