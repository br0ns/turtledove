open MLBGrammar

type comments = Source.Comments.t
val join = Tree.join
fun leaf c v = Tree.singleton $ c v

type node = string node
type tree = node Tree.t

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
       | basbinds of tree list
       | basbinds' of tree * tree list
       | basbinds'' of tree list
       | basdec of tree
       | basdecnode of tree
       | basdecs of tree list
       | basdecsnode of tree list
       | basexp of tree
       | basexpnode of tree
       | basid of basid
       | basids of basids
       | fctbinds of fctbinds
       | fctbinds' of fctid * fctbinds
       | fctbinds'' of fctbinds
       | fctid of fctid
       | id of basid
       | mlb of tree * comments
       | sigbinds of sigbinds
       | sigbinds' of sigid * sigbinds
       | sigbinds'' of sigbinds
       | sigid of sigid
       | strbinds of strbinds
       | strbinds' of strid * strbinds
       | strbinds'' of strbinds
       | strid of strid

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

mlb : basdecs (join Basdecs basdecs, Source.Comments.get source)

basdecs : basdecsnode (basdecsnode)

basdecsnode :                    (nil)
            | SEMICOLON basdecs  (basdecs)
            | basdec basdecs     (basdec :: basdecs)

basdec : basdecnode (basdecnode)

basdecnode : BASIS basbinds
                (join Dec_Basis basbinds)
           | LOCAL basdecs IN basdecs END
                (join Dec_Local
                      [join Basdecs basdecs1,
                       join Basdecs basdecs2])
           | OPEN basids
                (leaf Dec_Open basids)
           | FILE
                (leaf Dec_Source FILE)
           | STRING
                (leaf Dec_Source STRING)
           | ANN annPlus IN basdecs END
                (join (Dec_Ann annPlus) basdecs)
           | STRUCTURE strbinds
                (leaf Dec_Structure strbinds)
           | SIGNATURE sigbinds
                (leaf Dec_Signature sigbinds)
           | FUNCTOR fctbinds
                (leaf Dec_Functor fctbinds)
           | PRIM
                (Tree.singleton Prim)

fctbinds : fctid EQUALOP fctbinds'
                (let
                   val (fctid', fctbinds) = fctbinds'
                 in
                   (fctid, fctid') :: fctbinds
                 end)
         | fctid fctbinds''
                ((fctid, fctid) :: fctbinds'')

fctbinds' : fctid fctbinds'' (fctid, fctbinds'')

fctbinds'' :              (nil)
           | AND fctbinds (fctbinds)

sigbinds : sigid EQUALOP sigbinds'
                (let
                   val (sigid', sigbinds) = sigbinds'
                 in
                   (sigid, sigid') :: sigbinds
                 end)
         | sigid sigbinds''
                ((sigid, sigid) :: sigbinds'')

sigbinds' : sigid sigbinds'' (sigid, sigbinds'')

sigbinds'' :              (nil)
           | AND sigbinds (sigbinds)

strbinds : strid EQUALOP strbinds'
                (let
                   val (strid', strbinds) = strbinds'
                 in
                   (strid, strid') :: strbinds
                 end)
         | strid strbinds''
                ((strid, strid) :: strbinds'')

strbinds' : strid strbinds''
                (strid, strbinds'')

strbinds'' :              (nil)
           | AND strbinds (strbinds)

basbinds : basid EQUALOP basbinds'
                (let
                     val (basexp, basbinds) = basbinds'
                 in
                     join (Basbind basid) [basexp] :: basbinds
                 end)

basbinds' : basexp basbinds''  (basexp, basbinds'')

basbinds'' :               (nil)
           | AND basbinds  (basbinds)

basexp : basexpnode (basexpnode)

basexpnode : BAS basdecs END           (join Exp_Basis basdecs)
           | basid                     (leaf Exp_Var basid)
           | LET basdecs IN basexp END (join Exp_Let [join Basdecs basdecs, basexp])

basid : id (id)
basids : basid ([basid])
       | basid basids (basid :: basids)
fctid : id (id)
sigid : id (id)
strid : id (id)
id : ID (ID)


ann : STRING (STRING)

annPlus : ann annStar (ann :: annStar)

annStar :         (nil)
        | annPlus (annPlus)
