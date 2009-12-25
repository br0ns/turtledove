open MLBGrammar

fun makeSourceOrInclude file st pos =
    let
        val inc = ["mlb"]
        val src = ["ml", "sml", "sig", "fun"]
    in
        if      List.exists (fn s => String.isSuffix s file) inc then
            Include file
        else if List.exists (fn s => String.isSuffix s file) src then
            Source file
        else
            Source.lexError st pos "Unknown filetype."
    end

type comments = Source.Comments.t

%%

%name MLB

%term
      ID of string | COMMA | SEMICOLON | EOF
    | AND | BAS | BASIS | END | EQUALOP | FUNCTOR | IN | LET 
    | LOCAL | OPEN | SIGNATURE | STRUCTURE
    | ANN | PRIM | FILE of string | STRING of string

%nonterm
       (*   ann of string *)
       (* | annPlus of string list *)
       (* | annStar of string list *)
         basbinds of basbinds
       | basbinds' of basexp * basbinds
       | basbinds'' of basbinds
       | basdec of basdec
       | basdecnode of basdec
       | basdecs of basdecs
       | basdecsnode of basdecs
       | basexp of basexp
       | basexpnode of basexp
       | basid of basid
       | basids of basids
       (* | fctbinds of fctbinds *)
       (* | fctbinds' of Fctid.t * fctbinds *)
       (* | fctbinds'' of fctbinds *)
       (* | fctid of Fctid.t *)
       | id of basid
       | mlb of basdecs * comments
       (* | sigbinds of sigbinds *)
       (* | sigbinds' of Sigid.t * sigbinds *)
       (* | sigbinds'' of sigbinds *)
       (* | sigid of Sigid.t *)
       (* | strbinds of strbinds *)
       (* | strbinds' of Strid.t * strbinds *)
       (* | strbinds'' of strbinds *)
       (* | strid of Strid.t *)

%pos int
%arg (source) : Source.t
%eop EOF
%noshift EOF
%verbose

%right AND

%keyword AND BAS BASIS END FUNCTOR IN LET LOCAL OPEN SIGNATURE STRUCTURE ANN PRIM

%change -> SEMICOLON | -> IN ID END

%value ID ("<bogus>")

%%

mlb : basdecs (basdecs, Source.Comments.get source)

basdecs : basdecsnode (basdecsnode)

basdecsnode :                    (nil)
            | SEMICOLON basdecs  (basdecs)
            | basdec basdecs     (basdec :: basdecs)

basdec : basdecnode (basdecnode)

basdecnode : BASIS basbinds
                (Basis basbinds)
           | LOCAL basdecs IN basdecs END
                (Local (basdecs1, basdecs2))
           | OPEN basids
                (Open basids)
           | FILE
                (makeSourceOrInclude FILE source FILEleft)
           | STRING
                (makeSourceOrInclude STRING source STRINGleft)

(* basdecnode *)
(*    : FUNCTOR fctbinds *)
(*      (let  *)
(*          val fctbinds = Vector.fromList fctbinds *)
(*       in  *)
(*          Basdec.Defs (ModIdBind.makeRegion' (ModIdBind.Fct fctbinds, FUNCTORleft, fctbindsright)) *)
(*       end) *)
(*    | SIGNATURE sigbinds *)
(*      (let *)
(*          val sigbinds = Vector.fromList sigbinds *)
(*       in *)
(*          Basdec.Defs (ModIdBind.makeRegion' (ModIdBind.Sig sigbinds, SIGNATUREleft, sigbindsright)) *)
(*       end) *)
(*    | STRUCTURE strbinds *)
(*      (let *)
(*          val strbinds = Vector.fromList strbinds *)
(*       in *)
(*          Basdec.Defs (ModIdBind.makeRegion' (ModIdBind.Str strbinds, STRUCTUREleft, strbindsright)) *)
(*       end) *)
(*    | BASIS basbinds *)
(*      (let *)
(*          val basbinds = Vector.fromList basbinds *)
(*       in *)
(*          Basdec.Basis basbinds *)
(*       end) *)
(*    | LOCAL basdecs IN basdecs END  (Basdec.Local (basdecs1, basdecs2)) *)
(*    | OPEN basids  (Basdec.Open (Vector.fromList basids)) *)
(*    | FILE *)
(*      (let val reg = reg (FILEleft, FILEright) *)
(*       in lexAndParseProgOrMLB (FILE, reg) *)
(*       end) *)
(*    | STRING *)
(*      (let val reg = reg (STRINGleft, STRINGright) *)
(*       in lexAndParseProgOrMLB (STRING, reg) *)
(*       end) *)
(*    | PRIM (Basdec.Prim) *)
(*    | ANN annPlus IN basdecs END   *)
(*      (let  *)
(*         val extendRight =  *)
(*           let val right = valOf (Region.right (Basdec.region basdecs)) *)
(*           in fn reg => Region.extendRight (reg, right) *)
(*           end *)
(*         fun mkAnn' ((ann,reg), basdecs) = Basdec.Ann (ann, reg, basdecs) *)
(*         fun mkAnn ((ann,reg), basdecsnode) : Basdec.node = *)
(*           mkAnn' ((ann,reg), Basdec.makeRegion (basdecsnode, extendRight reg)) *)
(*         val (anns,ann) = List.splitLast annPlus *)
(*       in *)
(*         List.fold(anns, mkAnn'(ann, basdecs), mkAnn) *)
(*       end) *)

(* fctbinds : fctid EQUALOP fctbinds' *)
(*            (let val (def, fctbinds) = fctbinds' *)
(*             in {lhs = fctid, rhs = def} *)
(*                :: fctbinds *)
(*             end) *)
(*          | fctid fctbinds'' *)
(*            ({lhs = fctid, rhs = fctid} :: fctbinds'') *)

(* fctbinds' : fctid fctbinds''  (fctid, fctbinds'') *)

(* fctbinds'' :               ([]) *)
(*            | AND fctbinds  (fctbinds) *)

(* sigbinds : sigid EQUALOP sigbinds' *)
(*            (let val (def, sigbinds) = sigbinds' *)
(*             in {lhs = sigid, rhs = def} *)
(*                :: sigbinds *)
(*             end) *)
(*          | sigid sigbinds'' *)
(*            ({lhs = sigid, rhs = sigid} :: sigbinds'') *)

(* sigbinds' : sigid sigbinds''  (sigid, sigbinds'') *)

(* sigbinds'' :               ([]) *)
(*            | AND sigbinds  (sigbinds) *)

(* strbinds : strid EQUALOP strbinds' *)
(*            (let val (def, strbinds) = strbinds' *)
(*             in {lhs = strid, rhs = def} *)
(*                :: strbinds *)
(*             end) *)
(*          | strid strbinds'' *)
(*            ({lhs = strid, rhs = strid} :: strbinds'') *)

(* strbinds' : strid strbinds''  (strid, strbinds'') *)

(* strbinds'' :               ([]) *)
(*            | AND strbinds  (strbinds) *)

basbinds : basid EQUALOP basbinds'
                (let
                     val (basexp, basbinds) = basbinds'
                 in
                     (basid, basexp) :: basbinds
                 end)

basbinds' : basexp basbinds''  (basexp, basbinds'')

basbinds'' :               (nil)
           | AND basbinds  (basbinds)

basexp : basexpnode (basexpnode)

basexpnode : BAS basdecs END           (Bas basdecs)
           | basid                     (Var basid)
           | LET basdecs IN basexp END (Let (basdecs, basexp))

basid : id (id)
basids : basid ([basid])
       | basid basids (basid :: basids)
(* fctid : id  (Fctid.fromSymbol id) *)
(* sigid : id  (Sigid.fromSymbol id) *)
(* strid : id  (Strid.fromSymbol id) *)
id : ID (ID)


(* ann : STRING  (STRING, reg (STRINGleft, STRINGright)) *)

(* annPlus : ann annStar (ann::annStar) *)

(* annStar :          ([]) *)
(*         | annPlus  (annPlus) *)
