open SMLGrammar
type comments = Source.Comments.t
val dummypos = ~1

val join = Tree.join

fun wrap n l r = Wrap.wrap n {left = l, right = r}

type data = {left : int, right : int}
val data = Wrap.data o Tree.this
fun left t =
    case data t of
      {left, right} => left
fun right t =
    case data t of
      {left, right} => right
val unwrap = Wrap.unwrap

val leftmost = left o List.hd
val rightmost = right o List.last

val node = Wrap.unwrap o Tree.this
val children = Tree.children

fun error p s = raise ParseError (p, s)
fun die s = Crash.impossible s

val opify = Wrap.modify Ident.opify

fun getField t =
    case node t of
      Label_Plain field => field
    | Label_Short field => field
    | _ => die "getField"

fun reportDuplicates f p s xs =
    let
      fun loop nil = true
        | loop (x :: xs) = List.all (fn y => y <> x) xs andalso loop xs
    in
      if loop (map f xs) then
        xs
      else
        error p s
    end

fun ensureUnqual i =
    if Ident.isQual (unwrap i) then
      case Wrap.data i of
        {left, right} =>
        error left "Identifier must be unqualified"
    else
      i

fun mkIdent' l r f s = wrap (Ident.fromString f s) l r

fun mkIdent l r f s = ensureUnqual (mkIdent' l r f s)
fun mkLongIdent l r s = mkIdent' l r Fixity.Nonfix s

fun mkTyvar l r s =
    if String.sub (s, 0) = #"'" then
      mkIdent' l r Fixity.Nonfix s
    else
      error l "Type variable must start with a pling (')"


  %%
  %term
      CHAR of string
    | INT of string
    | LONGID of string
    | REAL of string
    | STRING of string
    | TYVAR of string
    | WORD of string
    | ABSTYPE | AND | ANDALSO | ARROW | AS | ASTERISK | BAR | CASE | COLON
    | COLONGT | COMMA | DATATYPE | DOTDOTDOT | ELSE | END | EOF | EQUALOP
    | EQTYPE | EXCEPTION | DO | DARROW | FN | FUN | FUNCTOR | HANDLE | HASH
    | IF | IN | INCLUDE | INFIX | INFIXR | LBRACE | LBRACKET | LET | LOCAL
    | LPAREN | NONFIX | ORELSE | OF | OP | OPEN | OVERLOAD | RAISE | RBRACE
    | RBRACKET | REC | RPAREN | SEMICOLON | SHARING | SIG | SIGNATURE | STRUCT
    | STRUCTURE | THEN | TYPE | VAL | WHERE | WHILE | WILD | WITH | WITHTYPE
      (* Extensions *)
    | BUILD_CONST | COMMAND_LINE_CONST | CONST
    | ADDRESS | EXPORT | IMPORT | SYMBOL
    | PRIM

%nonterm
         aexp of node * ast list
       | andspecs of ast list
       | apat of ast
       | apatnode of node * ast list
       | apats of ast list
       | app_exp of ast list
       | app_exp1 of ast list
       | arg_fct of ast
       | ieattributes of unit
       | clause of ast
       | clauses of ast list
       | clausesTop of ast
       | commapats of ast list
       | con of ident
       | const of SCon.t
       | const' of SCon.t
       | constr of ast
       | constraint of ast
       | constrs of ast list
       | constOrBool of SCon.t
       | datBind of ast list
       | datBindNoWithtype of ast list
       | datatypeRhs of node * ast list
       | datatypeRhsNoWithtype of node * ast list
       | datatypeRhsnode of node * ast list
       | datatypeRhsnodeNoWithtype of node * ast list
       | db of ast
       | dbs of ast list
       | dbs' of ast list
       | dec of ast
       | decnode of node * ast list * pos * pos
       | decnolocal of node * ast list * pos * pos
       | decs of ast list
       | digit of int
       | eb of ast
       | ebrhs of int -> int -> ident -> ast
       | ebrhsnode of int -> int -> ident -> ast
       | ebs of ast list
       | elabel of ast
       | elabels of ast list
       | exndesc of ast
       | exndescs of ast list
       | exp of ast
       | exp_2c of ast list
       | exp_list of ast list
       | exp_ps of ast list
       | expnode of node * ast list * pos * pos
       | expsAndTopdecs of ast list
       | fctarg of ast
       | fctid of ident
       | field of ident
       | fixity of Fixity.t
       | funbinds of ast list
       | funbinds' of ast * ast list
       | funbinds'1 of ast list * ast list
       | funbinds'1' of ast list * ast list
       | funbinds'2 of ast list
       | funs of ast list
       | id of ident
       | idEqual of ident
       | idNoAsterisk of ident
       | int of string
       | longcon of ident
       | longid of ident
       | longidEqual of ident
       | longidNoAsterisk of ident
       | longstrid of ident
       | longstrideqns of ident list
       | longstrids of ident list
       | longtycon of ident
       | longtyconeqns of ident list
       | longvidands of ident list
       | longvid of ident
       | longvidNoEqual of ident
       | match of ast
       | opaspat of ast
       | opcon of ident
       | pat of ast
       | patitem of ast
       | patitems of ast list * bool
       | patnode of node * ast list
       | pats of ast list
       | priority of int option
       | program of ast * comments
       | repl of ident * ident
       | rule of ast
       | rules of ast list
       | rvalbind of ast list
       | rvalbindRest of ast list
       | sharespec of node * ast list
       | sigbinds of ast list
       | sigbinds' of ast list * ast list
       | sigbinds'' of ast list * ast list
       | sigconst of ast
       | sigexp of ast
       | sigexp' of ast
       | sigexp'node of node * ast list
       | sigid of ident
       | sigids of ident list
       | spec of ast
       | specnode of node * ast list
       | specs of ast list
       | strbinds of ast list
       | strbinds' of ast * ast list
       | strbinds'1 of ast list * ast list
       | strbinds'1' of ast list * ast list
       | strbinds'2 of ast list
       | strdec of ast
       | strdecnode of node * ast list * pos * pos
       | strdecs of ast list
       | strdecsnode of ast list
       | strdescs of ast list
       | strdescs' of ast list * ast list
       | strdescs'' of ast list * ast list
       | strexp of ast
       | strexp1 of ast * Sigcon.t * ast
       | strexp2 of ast
       | strexp2node of node * ast list
       | strexpnode of node * ast list
       | strid of ident
       | string of string
       | symattributes of unit
       | tlabel of ast
       | tlabels  of ast list
       | topdec of ast
       | topdecnode of ast
       | topdecs of ast list
       | tuple_ty of ast list
       | ty of ast
       | ty' of ast
       | ty'node of node * ast list
       | ty0_pc of ast list
       | tyOpt of ast option
       | tycon of ident
       | tynode of node * ast list
       | typBind of ast list
       | typBind' of ast list
       | typBind'' of ast list
       | typdesc of ast
       | typdescs of ast list
       | tyvar of ident
       | tyvar_pc of ident list
       | tyvars of ident list
       | tyvarseq of ident list
       | valbind of ast list
       | valbindRest of ast list
       | valbindTop of ast list
       | valdesc of ast
       | valdescs of ast list
       | var of ident
       | vid of ident
       | vidNoEqual of ident
       | vids of ident list
       | wherespec of ast
       | wherespecs of ast list
       | wherespecs' of ast list
       | withtypes of ast list
       | word of string

%verbose
%pos int
%eop EOF
%noshift EOF

%arg (source) : Source.t

%nonassoc WITHTYPE
%right AND
%right ARROW
%right DARROW
%left DO
%left ELSE
%left RAISE
%right HANDLE
%left ORELSE
%left ANDALSO
%right AS
%left COLON

%name SML

%keyword ABSTYPE AND AS CASE DATATYPE DOTDOTDOT ELSE END
  EQTYPE EXCEPTION  DO  DARROW  FN  FUN  FUNCTOR  HANDLE
  IF IN INCLUDE  INFIX  INFIXR  LET  LOCAL  NONFIX  OF  OP
  OPEN OVERLOAD  RAISE  REC  SHARING  SIG  SIGNATURE  STRUCT
  STRUCTURE THEN TYPE VAL WHILE WHERE WITH WITHTYPE
  ORELSE ANDALSO

%change -> VAL | -> THEN | -> ELSE | -> LPAREN | -> SEMICOLON |
        DARROW -> EQUALOP | EQUALOP -> DARROW | AND -> ANDALSO | COLON -> OF |
        SEMICOLON -> COMMA | COMMA -> SEMICOLON |
        -> IN LONGID END | -> ELSE LONGID

%value CHAR ("a")
%value INT ("42")
%value LONGID ("bogus")
%value REAL ("42.0")
%value STRING ("bogus")
%value TYVAR ("'a")
%value WORD ("0w42")

%%

(* Non-terminals with epsilon productions:
 *  topdecs
expsAndTopdecs
strdecs
strdecsnode
strbinds'1
strbinds'2
sigbinds'
sigbinds''
sigconst
 *)

(* tree * comments *)
program : expsAndTopdecs
                ((join (wrap Topdecs
                             (leftmost expsAndTopdecs)
                             (rightmost expsAndTopdecs)
                       )
                       expsAndTopdecs,
                  Source.Comments.get source
                ))

(* tree list *)
expsAndTopdecs :
    exp SEMICOLON expsAndTopdecs
                ( exp :: expsAndTopdecs )
  | topdecs
                ( topdecs )

(* tree list *)
topdecs :
                ( nil )
  | topdec topdecs
                ( topdec :: topdecs )
  | SEMICOLON expsAndTopdecs
                ( expsAndTopdecs )

(* tree *)
topdec : topdecnode ( topdecnode )

(* tree *)
topdecnode :
     strdec
                ( strdec )
   | SIGNATURE sigbinds
                ( join (wrap Sigdec_Sig
                              SIGNATUREleft
                              (rightmost sigbinds)
                       )
                        sigbinds
                )
   | FUNCTOR funbinds
                ( join (wrap Fundec_Fun
                             FUNCTORleft
                             (rightmost funbinds)
                       )
                       funbinds
                )

(*---------------------------------------------------*)
(*                    Structures                     *)
(*---------------------------------------------------*)

(* tree list *)
strdecs : strdecsnode
                ( strdecsnode )

(* tree list *)
strdecsnode :
                ( nil )
   | SEMICOLON strdecs
                ( strdecs )
   | strdec strdecs
                ( strdec :: strdecs )

(* tree *)
strdec : strdecnode
                (let
                   val (node, children, l, r) = strdecnode
                 in
                   join (wrap node l r) children
                 end)

(* t * tree list * pos * pos *)
strdecnode :
     STRUCTURE strbinds
                ((Strdec_Str, strbinds, STRUCTUREleft, rightmost strbinds))
   | LOCAL strdecs IN strdecs END
                ((Strdec_Local,
                  [join (wrap Strdecs
                              (leftmost strdecs1)
                              (rightmost strdecs1)
                        )
                        strdecs1,
                   join (wrap Strdecs
                              (leftmost strdecs2)
                              (rightmost strdecs2)
                        )
                        strdecs2
                  ],
                  LOCALleft,
                  ENDright
                ))
   | decnolocal ( decnolocal )

(* tree list *)
strbinds : strid sigconst EQUALOP strbinds'
           ( let
              val (strexp, strbinds) = strbinds'
            in
              (join (wrap (Strbind strid)
                          stridleft
                          (right strexp)
                    )
                    [strexp, sigconst]
              ) :: strbinds
            end )

(* tree * tree list *)
strbinds' : strexp1 strbinds'1
            ( let
               val (strexp, sigcon, sigexp) = strexp1
               val (wherespecs, strbinds) = strbinds'1
               val sigexp' =
                   case wherespecs of
                     nil => sigexp
                   | _   =>
                         let
                           val l = leftmost wherespecs
                           val r = rightmost wherespecs
                         in
                           join (wrap Sigexp_Where
                                      (left sigexp)
                                      r
                                )
                                [sigexp,
                                 join (wrap Wherespecs l r)
                                      wherespecs
                                ]
                         end
             in
               (join (wrap Strexp_Con strexp1left (right sigexp'))
                     [strexp,
                      join (wrap (Sigcon sigcon)
                                 (left sigexp')
                                 (right sigexp')
                           )
                           [sigexp']
                     ],
                strbinds)
             end)
          | strexp2 strbinds'2
            ( (strexp2, strbinds'2) )

(* tree list * tree list *)
strbinds'1 : strbinds'2
               ( (nil, strbinds'2) )
           | WHERE wherespec strbinds'1'
             ( let
                val (wherespecs, strbinds) = strbinds'1'
              in
                (wherespec :: wherespecs, strbinds)
              end )

(* tree list * tree list *)
strbinds'1' : strbinds'1
                ( strbinds'1 )
            | AND wherespec strbinds'1'
              ( let
                 val (wherespecs, strbinds) = strbinds'1'
               in
                 (wherespec :: wherespecs, strbinds)
               end )

(* tree list *)
strbinds'2 :     ( nil )
           | AND strbinds
                 ( strbinds )

(* tree *)
strexp
  : strexpnode
      (let
         val (node, children) = strexpnode
       in
         join (wrap node strexpnodeleft strexpnoderight)
              children
       end)

(* t * tree list *)
strexpnode
  : strexp1
      (let
         val (strexp, sigcon, sigexp) = strexp1
       in
         (Strexp_Con,
          [strexp,
           join (wrap (Sigcon sigcon) (left sigexp) (right sigexp))
                [sigexp]
          ])
       end)
  | strexp1 wherespecs
      (let
         val (strexp, sigcon, sigexp) = strexp1
       in
         (Strexp_Con,
          [strexp,
           join (wrap (Sigcon sigcon) (left sigexp) wherespecsright)
                [join (wrap Sigexp_Where (left sigexp) wherespecsright)
                      [sigexp,
                       join (wrap Wherespecs wherespecsleft wherespecsright) wherespecs]
                ]
          ])
       end)
  | strexp2node
      (strexp2node)

(* tree * Sigcon.t * tree *)
strexp1
  : strexp COLON sigexp'
           ((strexp, Sigcon.Transparent, sigexp'))
  | strexp COLONGT sigexp'
           ((strexp, Sigcon.Opaque, sigexp'))

(* tree *)
strexp2
  : strexp2node
      (let
         val (node, children) = strexp2node
       in
         join (wrap node strexp2nodeleft strexp2noderight)
              children
       end)

(* t * tree list *)
strexp2node
  : longid
      ((Strexp_Var longid, nil))
  | STRUCT strdecs END
      ((Strexp_Struct, strdecs))
  | longid arg_fct
      ((Strexp_Fun longid, [arg_fct]))
  | LET strdecs IN strexp END
      ((Strexp_Let,
        [join (wrap Strdecs
                    (leftmost strdecs)
                    (rightmost strdecs)
              ) strdecs,
         strexp]
      ))

(* tree *)
arg_fct
  : LPAREN strexp RPAREN
      (strexp)
  | LPAREN strdecs RPAREN
      (join (wrap Strdecs
                  (leftmost strdecs)
                  (rightmost strdecs)
            ) strdecs)

(*---------------------------------------------------*)
(*                    Signatures                     *)
(*---------------------------------------------------*)

(* tree *)
sigexp
  : sigexp'
      (sigexp')
  | sigexp' wherespecs
      (join (wrap Sigexp_Where sigexp'left wherespecsright)
            [sigexp',
             join (wrap Wherespecs wherespecsleft wherespecsright) wherespecs]
      )

(* tree list *)
wherespecs
  : wherespecs'
      (wherespecs')

(* tree list *)
wherespecs'
  : WHERE wherespec
      ([wherespec])
  | WHERE wherespec wherespecs'
      (wherespec :: wherespecs')
  | WHERE wherespec andspecs
      (wherespec :: andspecs)

(* tree list *)
andspecs
  : AND wherespec
      ([wherespec])
  | AND wherespec andspecs
      (wherespec :: andspecs)
  | AND wherespec wherespecs'
      (wherespec :: wherespecs')

(* tree list *)
sigbinds : sigid EQUALOP sigexp' sigbinds'
      (let
         val (wherespecs, sigbinds) = sigbinds'
         val sigexp =
             case wherespecs of
               nil => sigexp'
             | _   =>
               let
                 val leftmost = left (List.hd wherespecs)
                 val rightmost = right (List.last wherespecs)
               in
                 join (wrap Sigexp_Where (left sigexp') rightmost)
                      [sigexp',
                       join (wrap Wherespecs leftmost rightmost)
                            wherespecs
                      ]
               end
       in
         join (wrap (Sigbind sigid) sigidleft (right sigexp))
              [sigexp]
         :: sigbinds
       end)

(* tree *)
sigexp'
  : sigexp'node
      (let
         val (node, children) = sigexp'node
       in
         join (wrap node sigexp'nodeleft sigexp'noderight) children
       end)

(* t * tree list *)
sigexp'node
  : sigid
      ((Sigexp_Var sigid, nil))
  | SIG specs END
      ((Sigexp_Spec, specs))

(* tree list * tree list *)
sigbinds' :
      ((nil, nil))
  | AND sigbinds
      ((nil, sigbinds))
  | WHERE wherespec sigbinds''
      (let
         val (wherespecs, sigbinds) = sigbinds''
       in
         (wherespec :: wherespecs, sigbinds)
       end)

(* tree list * tree list *)
sigbinds''
  : sigbinds'
      (sigbinds')
  | AND wherespec sigbinds''
      (let
         val (wherespecs, sigbinds) = sigbinds''
       in
         (wherespec :: wherespecs, sigbinds)
       end)

(* tree *)
wherespec
  : TYPE tyvars longtycon EQUALOP ty
      (join (wrap (Wherespec (tyvars, longtycon)) TYPEleft tyright)
            [ty]
      )

(* tree *)
sigconst :
      (join (wrap (Sigcon Sigcon.None) dummypos dummypos) nil)
  | COLON sigexp
      (join (wrap (Sigcon Sigcon.Transparent) sigexpleft sigexpright)
            [sigexp]
      )
  | COLONGT sigexp
      (join (wrap (Sigcon Sigcon.Opaque) sigexpleft sigexpright)
            [sigexp]
      )

(* tree list *)
specs :
      (nil)
  | SEMICOLON specs
      (specs)
  | spec specs
      (spec :: specs)

(* tree *)
spec
  : specnode
      (let
         val (node, children) = specnode
       in
         join (wrap node specnodeleft specnoderight)
              children
       end)

(* t * tree list *)
specnode
  : VAL valdescs
      ((Spec_Val, valdescs))
  | TYPE typdescs
      ((Spec_Type, typdescs))
  | TYPE typBind
      ((Spec_Typedef, typBind))
  | EQTYPE typdescs
      ((Spec_EqType, typdescs))
  | DATATYPE datatypeRhsNoWithtype
      (datatypeRhsNoWithtype)
  | EXCEPTION exndescs
      ((Spec_Exception, exndescs))
  | STRUCTURE strdescs
      ((Spec_Structure, strdescs))
  | INCLUDE sigexp
      ((Spec_Include, [sigexp]))
  | INCLUDE sigid sigids (* p. 59 *)
      ((Spec_IncludeSigids (sigid :: sigids), nil))
  | sharespec
      (sharespec)

(* t * tree list *)
sharespec
  : SHARING TYPE longtyconeqns
      ((Spec_Sharing longtyconeqns, nil))
  | SHARING longstrideqns
      ((Spec_SharingStructure longstrideqns, nil))

(* ident list *)
longstrideqns
  : longstrid EQUALOP longstrid
      ([longstrid1, longstrid2])
  | longstrid EQUALOP longstrideqns
      (longstrid :: longstrideqns)

(* ident list *)
longtyconeqns
  : longtycon EQUALOP longtycon
      ([longtycon1, longtycon2])
  | longtycon EQUALOP longtyconeqns
      (longtycon :: longtyconeqns)

(* tree list *)
strdescs
  : strid COLON sigexp' strdescs'
      (let
         val (wherespecs, strdescs) = strdescs'
         val sigexp =
             case wherespecs of
               nil => sigexp'
             | _   =>
               let
                 val leftmost = left (List.hd wherespecs)
                 val rightmost = right (List.last wherespecs)
               in
                 join (wrap Sigexp_Where (left sigexp') rightmost)
                      [sigexp',
                       join (wrap Wherespecs leftmost rightmost)
                            wherespecs
                      ]
               end
       in
         join (wrap (Strdesc strid) stridleft (right sigexp))
              [sigexp]
         :: strdescs
       end)

(* tree list * tree list *)
strdescs' :
      ((nil, nil))
  | AND strdescs
      ((nil, strdescs))
  | WHERE wherespec strdescs''
      (let
         val (wherespecs, strdescs) = strdescs''
       in
         (wherespec :: wherespecs, strdescs)
       end)

(* tree list * tree list *)
strdescs''
  : strdescs'
      (strdescs')
  | AND wherespec strdescs''
      (let
         val (wherespecs, strdescs) = strdescs''
       in
         (wherespec :: wherespecs, strdescs)
       end)

(* tree list *)
typdescs
  : typdesc
      ([typdesc])
  | typdesc AND typdescs
      (typdesc :: typdescs)

(* tree *)
typdesc
  : tyvars tycon
      (join (wrap (Tydesc (tyvars, tycon)) tyvarsleft tyconright)
            nil
      )

(* tree list *)
valdescs
  : valdesc
      ([valdesc])
  | valdesc AND valdescs
      (valdesc :: valdescs)

(* tree *)
valdesc
  : var COLON ty
      (join (wrap (Valdesc var) varleft tyright)
            [ty]
      )

(* tree list *)
exndescs
  : exndesc
      ([exndesc])
  | exndesc AND exndescs
      (exndesc :: exndescs)

(* tree *)
exndesc
  : con tyOpt
      (join (wrap (Exndesc con) conleft tyOptright)
       [case tyOpt of
         SOME ty => join (wrap MaybeTy tyOptleft tyOptright) [ty]
       | NONE    => join (wrap MaybeTy dummypos dummypos) nil
       ]
      )

(* tree option *)
tyOpt :
      (NONE)
  | OF ty
      (SOME ty)

(*---------------------------------------------------*)
(*                     Functors                      *)
(*---------------------------------------------------*)

(* tree list *)
funbinds
  : fctid LPAREN fctarg RPAREN sigconst EQUALOP funbinds'
      (let
         val (strexp, funbinds) = funbinds'
       in
         (join (wrap (Funbind fctid) fctidleft (right strexp))
               [fctarg, strexp, sigconst]
         ) :: funbinds
       end)

(* tree * tree list *)
funbinds'
  : strexp1 funbinds'1
      (let
         val (strexp, sigcon, sigexp) = strexp1
         val (wherespecs, funbinds) = funbinds'1
         val sigexp' =
             case wherespecs of
               nil => sigexp
             | _   =>
                   let
                     val leftmost = left (List.hd wherespecs)
                     val rightmost = right (List.last wherespecs)
                   in
                     join (wrap Sigexp_Where
                                (left sigexp)
                                rightmost
                          )
                          [sigexp,
                           join (wrap Wherespecs leftmost rightmost)
                                wherespecs
                          ]
                   end
       in
         (join (wrap Strexp_Con strexp1left (right sigexp'))
               [strexp,
                join (wrap (Sigcon sigcon)
                           (left sigexp')
                           (right sigexp')
                     )
                     [sigexp']
               ],
          funbinds)
       end)
  | strexp2 funbinds'2
      ((strexp2, funbinds'2))

(* tree list * tree list *)
funbinds'1
  : funbinds'2
      ((nil, funbinds'2))
  | WHERE wherespec funbinds'1'
      (let
         val (wherespecs, funbinds) = funbinds'1'
       in
         (wherespec :: wherespecs, funbinds)
       end)

(* tree list *)
funbinds'2 :
      (nil)
  | AND funbinds
      (funbinds)

(* tree list * tree list *)
funbinds'1'
  : funbinds'1
      (funbinds'1)
  | AND wherespec funbinds'1'
      (let
         val (wherespecs, funbinds) = funbinds'1'
       in
         (wherespec :: wherespecs, funbinds)
       end)

(* tree *)
fctarg
  : strid COLON sigexp
      (join (wrap (Funarg_Structure strid) stridleft sigexpright)
            [sigexp]
      )
  | specs
      (join (wrap Funarg_Spec specsleft specsright)
            specs
      )

(*---------------------------------------------------*)
(*                   Declarations                    *)
(*---------------------------------------------------*)

(* tree list *)
decs :
      (nil)
  | dec decs
      (dec :: decs)
  | SEMICOLON decs
      (decs)

(* tree *)
dec
  : decnode
      (let
         val (node, children, l, r) = decnode
       in
         join (wrap node l r)
              children
       end)

(* t * tree list * pos * pos *)
decnode
  : decnolocal
      (decnolocal)
  | LOCAL decs IN decs END
      ((Dec_Local,
        [join (wrap Decs decs1left decs1right) decs1,
         join (wrap Decs decs2left decs2right) decs2],
        LOCALleft,
        ENDright
      ))

(* t * tree list * pos * pos *)
decnolocal
  : VAL valbindTop
      ((Dec_Val nil, valbindTop, VALleft, rightmost valbindTop))
  | VAL tyvarseq valbindTop
      ((Dec_Val tyvarseq, valbindTop, VALleft, rightmost valbindTop))
  | FUN funs
      ((Dec_Fun nil, funs, FUNleft, rightmost funs))
  | FUN tyvarseq funs
      ((Dec_Fun tyvarseq, funs, FUNleft, rightmost funs))
  | TYPE typBind
      ((Dec_Type, typBind, TYPEleft, rightmost typBind))
  | DATATYPE datatypeRhs
      (let
         val (n, ts) = datatypeRhs
       in
         (n, ts, DATATYPEleft, datatypeRhsright)
       end)
  | ABSTYPE datBind WITH decs END
      (case datBind of
         [datbinds, withtypes] =>
         (Dec_Abstype,
          [datbinds,
           withtypes,
           join (wrap Decs decsleft decsright) decs
          ],
          ABSTYPEleft,
          ENDright)
       | _ => die "decnolocal"
      )
  | EXCEPTION ebs
      ((Dec_Exception, ebs, EXCEPTIONleft, ebsright))
  | OPEN longstrids
      ((Dec_Open longstrids, nil, OPENleft, longstridsleft))
  | fixity vids
      ((Dec_Fix (fixity, vids), nil, fixityleft, vidsright))
  | OVERLOAD priority var COLON ty AS longvidands
      ((Dec_Overload (priority, var, longvidands),
        [ty],
        OVERLOADleft,
        longvidandsright))

(* tree list *)
valbindTop
  : valbind
      (valbind)

(* tree list *)
valbind
  : pat EQUALOP exp valbindRest
      (join (wrap Valbind_Plain patleft expright) [pat, exp]
       :: valbindRest)
  | REC rvalbind
      (rvalbind)

(* tree list *)
valbindRest :
      (nil)
  | AND valbind
      (valbind)

(* tree list *)
rvalbind
  : REC rvalbind
      (rvalbind)
  | pat EQUALOP FN match rvalbindRest
      (join (wrap Valbind_Rec patleft matchright) [pat, match]
       :: rvalbindRest)

(* tree list *)
rvalbindRest :
      ([])
  | AND rvalbind
      (rvalbind)

(* tree *)
constraint :
      (join (wrap MaybeTy dummypos dummypos) nil)
  | COLON ty
      (join (wrap MaybeTy tyleft tyright) [ty])

(* tree list *)
funs
  : clausesTop
      ([clausesTop])
  | clausesTop AND funs
      (clausesTop :: funs)

(* tree *)
clausesTop
  : clauses
      (join (wrap Match clausesleft (rightmost clauses)) clauses)

(* tree list *)
clauses
  : clause
      ([clause])
  | clause BAR clauses
      (clause :: clauses)

(* tree *)
clause
  : apats constraint EQUALOP exp
      (join (wrap FlatClause apatsleft (right exp))
            [join (wrap Pats apatsleft apatsright) apats, constraint, exp])

(* tree list *)
typBind
  : typBind'
      (typBind')

(* tree list *)
typBind'
  : tyvars tycon EQUALOP ty typBind''
      (join (wrap (Tybind (tyvars, tycon)) tyvarsleft tyright) [ty] :: typBind'')

(* tree list *)
typBind'' :
      (nil)
  | AND typBind'
      (typBind')

(* ident list *)
tyvars
  : tyvarseq
      (tyvarseq)
  |   (nil)

(* ident list *)
tyvarseq
  : tyvar
      ([tyvar])
  | LPAREN tyvar_pc RPAREN
      (reportDuplicates (fn x => x) tyvar_pcleft "type variables" tyvar_pc)

(* ident list *)
tyvar_pc
  : tyvar
      ([tyvar])
  | tyvar COMMA tyvar_pc
      (tyvar :: tyvar_pc)

(* tree list *)
constrs
  : constr
      ([constr])
  | constr BAR constrs
      (constr :: constrs)

(* tree *)
constr
  : opcon
      (join (wrap (Constructor opcon) opconleft opconright)
            [join (wrap MaybeTy dummypos dummypos) nil])
  | opcon OF ty
      (join (wrap (Constructor opcon) opconleft tyright)
            [join (wrap MaybeTy tyleft tyright) [ty]])

(* ident *)
opcon
  : con
      (con)
  | OP con
      (opify con)

(* tree list *)
ebs
  : eb
      ([eb])
  | eb AND ebs
      (eb::ebs)

(* tree *)
eb
  : opcon ebrhs
      (ebrhs opconleft ebrhsright opcon)

(* int -> int -> ident -> tree *)
ebrhs
  : ebrhsnode
      (ebrhsnode)

(* int -> int -> ident -> tree *)
ebrhsnode :
      (fn l => fn r => fn i =>
         join (wrap (Constructor i) l r) nil
      )
  | OF ty
      (fn l => fn r => fn i =>
         join (wrap (Constructor i) l r) [ty]
      )
  | EQUALOP longcon
      (fn l => fn r => fn i =>
         join (wrap (Replication (i, longcon)) l r) nil
      )
  | EQUALOP OP longcon
      (fn l => fn r => fn i =>
         join (wrap (Replication (i, opify longcon)) l r) nil
      )

(* Fixity.t *)
fixity
  : INFIX
      (Fixity.InfixL NONE)
  | INFIX digit
      (Fixity.InfixL (SOME digit))
  | INFIXR
      (Fixity.InfixR NONE)
  | INFIXR digit
      (Fixity.InfixR (SOME digit))
  | NONFIX
      (Fixity.Nonfix)

(* int option *)
priority :
      (NONE)
  | digit
      (SOME digit)

(* string *)
int
  : INT
      (INT)

(* string *)
word
  : WORD
      (WORD)

(* int *)
digit
  : INT
      (case (Int.fromString INT, String.size INT) of
         (SOME n, 1) => n
       | _ => Source.error source INTleft "Invalid digit in infix declaration."
      )

(* t * tree list *)
datatypeRhs
  : datatypeRhsnode
      (datatypeRhsnode)

(* t * tree list *)
datatypeRhsNoWithtype
  : datatypeRhsnodeNoWithtype
      (datatypeRhsnodeNoWithtype)

(* t * tree list *)
datatypeRhsnode
  : repl
      ((Dec_Replication repl, nil))
  | datBind
      ((Dec_Datatype, datBind))

(* t * tree list *)
datatypeRhsnodeNoWithtype
  : repl
      ((Spec_Replication repl, nil))
  | datBindNoWithtype
      ((Spec_Datatype, datBindNoWithtype))

(* ident * ident *)
repl
  : tyvars tycon EQUALOP DATATYPE longtycon
      (if List.length tyvars = 0 then
         (tycon, longtycon)
       else
         Source.error source tyvarsleft "Type variables in datatype replication."
      )

(* tree list *)
datBind
  : dbs withtypes
      ([join (wrap Datatypes dbsleft dbsright) dbs,
        join (wrap Withtypes withtypesleft withtypesright) withtypes]
      )

(* tree list *)
datBindNoWithtype
  : dbs
      (dbs)

(* tree list *)
dbs
  : dbs'
      (dbs')

(* tree list *)
dbs'
  : db
      ([db])
  | db AND dbs'
      (db :: dbs')

(* tree *)
db
  : tyvars tycon EQUALOP constrs
      (join (wrap (Datatype (tyvars, tycon)) tyvarsleft constrsright) constrs)

(* tree list *)
withtypes :
      (nil)
  | WITHTYPE typBind
      (typBind)

(* ident list *)
longvidands
  : longvid
      ([longvid])
  | longvid AND longvidands
      (longvid :: longvidands)

(* tree *)
match
  : rules
      (join (wrap Match rulesleft (rightmost rules)) rules)

(* tree list *)
rules
  : rule
      ([rule])
  | rule BAR rules
      (rule :: rules)

(* tree *)
rule
  : pat DARROW exp
      (join (wrap Rule patleft (right exp)) [pat, exp])

(* tree *)
elabel
  : field EQUALOP exp
      (join (wrap (Label_Plain field) fieldleft expright) [exp])

(* tree list *)
elabels
  : elabel COMMA elabels
      (elabel :: elabels)
  | elabel
      ([elabel])

(* tree list *)
exp_ps
  : exp SEMICOLON exp
      ([exp1, exp2])
  | exp SEMICOLON exp_ps
      (exp :: exp_ps)

(* tree *)
exp
  : expnode
      (let
         val (node, children, l, r) = expnode
       in
         join (wrap node l r) children
       end)

(* t * tree list * pos * pos*)
expnode
  : exp HANDLE match
      ((Exp_Handle, [exp, match], left exp, right match))
  | exp ORELSE exp
      ((Exp_Orelse, [exp1, exp2], left exp1, right exp2))
  | exp ANDALSO exp
      ((Exp_Andalso, [exp1, exp2], left exp1, right exp2))
  | exp COLON ty
      ((Exp_Typed, [exp, ty], left exp, tyright))
  | app_exp
      ((Exp_FlatApp, app_exp, leftmost app_exp, rightmost app_exp))
  | FN match
      ((Exp_Fn, [match], FNleft, right match))
  | CASE exp OF match
      ((Exp_Case, [exp, match], CASEleft, right match))
  | WHILE exp DO exp
      ((Exp_While, [exp1, exp2], WHILEleft, right exp))
  | IF exp THEN exp ELSE exp
      ((Exp_If, [exp1, exp2, exp3], IFleft, right exp))
  | RAISE exp
      ((Exp_Raise, [exp], RAISEleft, right exp))

(* tree list *)
app_exp
  : aexp app_exp1
      (let
         val (node, children) = aexp
       in
         join (wrap node aexpleft aexpright) children :: app_exp1
       end)
  | longvid app_exp1
      (join (wrap (Exp_Var longvid) longvidleft longvidright) nil :: app_exp1)

(* tree list *)
app_exp1 :
      (nil)
  | app_exp
      (app_exp)

(* t * tree list *)
aexp
  : OP longvid
      ((Exp_Var (opify longvid), nil))
  | const
      ((Exp_SCon const, nil))
  | HASH field
      ((Exp_Selector field, nil))
  | LBRACE elabels RBRACE
      ((Exp_Record, elabels))
  | LBRACE RBRACE
      ((Exp_Record, nil))
  | LPAREN RPAREN
      ((Exp_Tuple, nil))
  | LPAREN exp RPAREN (* Changed: expnode -> exp *)
      ((Exp_Par, [exp]))
  | LPAREN exp_ps RPAREN
      ((Exp_Seq, exp_ps))
  | LPAREN exp_2c RPAREN
      ((Exp_Tuple, exp_2c))
  | LBRACKET exp_list RBRACKET
      ((Exp_List, exp_list))
  | LBRACKET RBRACKET
      ((Exp_List, nil))
  | LET decs IN exp END
      ((Exp_Let, [join (wrap Decs decsleft decsright) decs, exp]))
  | LET decs IN exp_ps END
      ((Exp_LetSeq, [join (wrap Decs decsleft decsright) decs,
                     join (wrap Exps exp_psleft exp_psright) exp_ps]
      ))
  | ADDRESS string COLON ty SEMICOLON
      (die "aexp")
  | BUILD_CONST string COLON ty SEMICOLON
      (die "aexp")
  | COMMAND_LINE_CONST string COLON ty EQUALOP constOrBool SEMICOLON
      (die "aexp")
  | CONST string COLON ty SEMICOLON
      (die "aexp")
  | EXPORT string ieattributes COLON ty SEMICOLON
      (die "aexp")
  | IMPORT string ieattributes COLON ty SEMICOLON
      (die "aexp")
  | IMPORT ASTERISK ieattributes COLON ty SEMICOLON
      (die "aexp")
  | PRIM string COLON ty SEMICOLON
      (die "aexp")
  | SYMBOL string symattributes COLON ty SEMICOLON
      (die "aexp")
  | SYMBOL ASTERISK COLON ty SEMICOLON
      (die "aexp")

ieattributes
  :
      (die "ieattributes")
  | id ieattributes
      (die "ieattributes")

symattributes
  :
      (die "symattributes")
  | id symattributes
      (die "symattributes")

(* tree list *)
exp_2c
  : exp COMMA exp_2c
      (exp :: exp_2c)
  | exp COMMA exp
      ([exp1, exp2])

(* tree list *)
exp_list
  : exp
      ([exp])
  | exp COMMA exp_list
      (exp :: exp_list)

(*---------------------------------------------------*)
(*                     Patterns                      *)
(*---------------------------------------------------*)

(* tree *)
pat
  : patnode
      (let
         val (node, children) = patnode
       in
         join (wrap node patnodeleft patnoderight) children
       end)

(* t * tree list *)
patnode
  : pat AS pat
      (case (node pat1, map node (children pat1)) of
         (Pat_FlatApp, [Pat_Var i]) =>
         (Pat_Layered i, [pat2])
       | _ =>
         Source.error
           source
           patleft
           "Left side of layered pattern must be an identifier."
      )
  | pat COLON ty
      ((Pat_Typed, [pat, ty]))
  | apats
      ((Pat_FlatApp, apats))

(* tree list *)
apats
  : apat
      ([apat])
  | apat apats
      (apat :: apats)

(* tree *)
apat
  : apatnode
      (let
         val (node, children) = apatnode
       in
         join (wrap node apatnodeleft apatnoderight) children
       end)

(* t * tree list *)
apatnode
  : longvidNoEqual
      ((Pat_Var longvidNoEqual, nil))
  | OP longvid
      ((Pat_Var (opify longvid), nil))
  | const
      ((Pat_SCon (case const of
                    SCon.Real _ =>
                    Source.error
                      source
                      constleft
                      "Real constants are not allowed in patterns."
                  | _ => const),
        nil))
  | WILD
      ((Pat_Wild, nil))
  | LPAREN pats RPAREN
      ((case pats of
         [pat] => Pat_Par
       | _     => Pat_Tuple
      , pats))
  | LBRACKET pats RBRACKET
      ((Pat_List, pats))
  | LBRACE RBRACE
      ((Pat_Tuple, nil))
  | LBRACE patitems RBRACE
      (let
         val (items, flexible) = patitems
         val _ = reportDuplicates
                   getField
                   patitemsleft
                   "Duplicate fields in record pattern."
                   items
       in
         (if flexible then
            Pat_FlexibleRecord
          else
            Pat_Record
          , items)
       end)

(* tree list *)
pats :
      (nil)
  | pat commapats
      (pat :: commapats)

(* tree list *)
commapats :
      (nil)
  | COMMA pat commapats
      (pat :: commapats)

(* tree list * bool *)
patitems
  : patitem COMMA patitems
      (let
         val (items, f) = patitems
       in
         (patitem :: items, f)
       end)
  | patitem
      ([patitem], false)
  | DOTDOTDOT
      (nil, true)

(* tree *)
patitem
  : field EQUALOP pat
      (join (wrap (Label_Plain field) fieldleft patright) [pat])
  | vidNoEqual constraint opaspat
      (join (wrap (Label_Short vidNoEqual) vidNoEqualleft opaspatright)
            [constraint, opaspat]
      )

(* tree *)
opaspat :
      (join (wrap MaybePat dummypos dummypos) nil)
  | AS pat
      (join (wrap MaybePat patleft patright) [pat])

(*---------------------------------------------------*)
(*                       Types                       *)
(*---------------------------------------------------*)

(* tree *)
ty
  : tynode
      (let
         val (node, children) = tynode
       in
         join (wrap node tynodeleft tynoderight) children
       end)

(* t * tree list *)
tynode
  : tuple_ty
      ((Ty_Tuple, tuple_ty))
  | ty ARROW ty
      ((Ty_Arrow, [ty1, ty2]))
  | ty'node
      (ty'node)

(* tree *)
ty'
  : ty'node
      (let
         val (node, children) = ty'node
       in
         join (wrap node ty'nodeleft ty'noderight) children
       end)

(* t * tree list *)
ty'node
  : tyvar
      ((Ty_Var tyvar, nil))
  | LBRACE tlabels RBRACE
      ((Ty_Record,
        let
          val _ = reportDuplicates
                    getField
                    tlabelsleft
                    "Duplicate fields in record type."
                    tlabels
        in
          tlabels
        end))
  | LBRACE RBRACE
      ((Ty_Record, nil))
  | LPAREN ty0_pc RPAREN longtycon
      ((Ty_Con longtycon, ty0_pc))
  | LPAREN ty RPAREN
      ((Ty_Par, [ty]))
  | ty' longtycon
      ((Ty_Con longtycon, [ty']))
  | longtycon
      ((Ty_Con longtycon, nil))

(* tree *)
tlabel
  : field COLON ty
      (join (wrap (Label_Plain field) fieldleft tyright) [ty])

(* tree list *)
tlabels
  : tlabel COMMA tlabels
      (tlabel :: tlabels)
  | tlabel
      ([tlabel])

(* tree list *)
tuple_ty
  : ty' ASTERISK tuple_ty
      (ty' :: tuple_ty)
  | ty' ASTERISK ty'
      ([ty'1, ty'2])

(* tree list *)
ty0_pc
  : ty COMMA ty
      ([ty1, ty2])
  | ty COMMA ty0_pc
      (ty :: ty0_pc)

(*---------------------------------------------------*)
(*                       Atoms                       *)
(*---------------------------------------------------*)

(* SCon.t *)
constOrBool
  : const
      (const)
  | id
      (SCon.String "Dummy")

(* SCon.t *)
const
  : const'
      (const')

(* SCon.t *)
const'
  : int
      (SCon.Int int)
  | word
      (SCon.Word word)
  | REAL
      (SCon.Real REAL)
  | STRING
      (SCon.String STRING)
  | CHAR
      (SCon.Char CHAR)

(* string *)
string
  : STRING
      (STRING)

(* ident *)
idNoAsterisk
  : longidNoAsterisk
      (ensureUnqual longidNoAsterisk)

(* ident *)
id
  : idNoAsterisk
      (idNoAsterisk)
  | ASTERISK
      (wrap Ident.Symbols.asterisk ASTERISKleft ASTERISKright)

(* ident *)
idEqual
  : id
      (id)
  | EQUALOP
      (wrap Ident.Symbols.equal EQUALOPleft EQUALOPright)

(* ident *)
longid
  : longidNoAsterisk
      (longidNoAsterisk)
  | ASTERISK
      (wrap Ident.Symbols.asterisk ASTERISKleft ASTERISKright)

(* ident *)
longidNoAsterisk
  : LONGID
      (mkLongIdent LONGIDleft LONGIDright LONGID
       handle ParseError (p, s) => Source.error source p s)

(* ident *)
longidEqual
  : longid
      (longid)
  | EQUALOP
      (wrap Ident.Symbols.equal EQUALOPleft EQUALOPright)

(* ident *)
vid
  : idEqual
      (idEqual)

(* ident *)
vidNoEqual
  : id
      (id)

(* ident list *)
vids
  : vid
      ([vid])
  | vid vids
      (vid::vids)

(* ident *)
var
  : idEqual
      (idEqual)

(* ident *)
con
  : id
      (id)

(* ident *)
tycon
  : idNoAsterisk
      (idNoAsterisk)

(* ident *)
tyvar
  : TYVAR
      (mkTyvar TYVARleft TYVARright TYVAR
       handle ParseError (p, s) => Source.error source p s)

(* ident *)
field
  : id
      (id)
  | int
      (mkIdent intleft intright Fixity.Nonfix int
      handle ParseError (p, s) => Source.error source p s)

(* ident *)
strid
  : id
      (id)

(* ident *)
sigid
  : id
      (id)

(* ident list *)
sigids
  : sigid
      ([sigid])
  | sigid sigids
      (sigid :: sigids)

(* ident *)
fctid
  : id
      (id)

(* ident *)
longtycon
  : longidNoAsterisk
      (longidNoAsterisk)

(* ident *)
longvid
  : longidEqual
      (longidEqual)

(* ident *)
longvidNoEqual
  : longid
      (longid)

(* ident *)
longcon
  : longid
      (longid)

(* ident *)
longstrid
  : longid
      (longid)

(* ident list *)
longstrids
  : longstrid
      ([longstrid])
  | longstrid longstrids
      (longstrid :: longstrids)

