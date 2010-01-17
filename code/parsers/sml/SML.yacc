type int = Int.t

fun reg (left, right) = Region.make {left = left, right = right}
fun error (reg, msg) = Control.error (reg, Layout.str msg, Layout.empty)

open Ast
structure Field = Record.Field
structure Srecord = SortedRecord

structure Type =
   struct
      open Type

      val tuple = Record o Srecord.tuple

      val unit = tuple (Vector.new0 ())

      fun arrow (t1, t2) = Con (Longtycon.arrow, Vector.new2 (t1, t2))
   end

structure DatBind =
   struct
      open DatBind

      fun make (dbs, withtypes, left, right) =
         makeRegion' (T {datatypes = dbs, withtypes = withtypes},
                      left, right)
   end

structure Pat =
   struct
      open Pat

      fun tuple ps =
         if 1 = Vector.length ps
            then node (Vector.sub (ps, 0))
         else Tuple ps

      val unit = tuple (Vector.new0 ())

      val bogus = unit

      fun makeAs (p1: t, p2: t): node =
         let
            fun err () =
               error (Pat.region p1, "must have variable to left in as pattern")
            fun fixopVar (p : t) =
               case node p of
                  FlatApp ps =>
                     if 1 = Vector.length ps
                        then (case node (Vector.sub (ps, 0)) of
                                 Var {fixop,name} =>
                                    (case Longvid.split name of
                                        ([], vid) =>
                                           SOME (fixop, Vid.toVar vid)
                                      | _ =>
                                           let
                                              val () = err ()
                                           in
                                              SOME (Fixop.None, Var.bogus)
                                           end)
                               | _ => NONE)
                     else NONE
                | _ => NONE
         in
            case fixopVar p1 of
               SOME (fixop, var) =>
                  Layered {fixop = fixop, var = var,
                           constraint = NONE,
                           pat = p2}
             | NONE =>
                  case node p1 of
                     Pat.Constraint (p, t) =>
                        (case fixopVar p of
                            SOME (fixop, var) =>
                               Layered {fixop = fixop, var = var,
                                        constraint = SOME t,
                                        pat = p2}
                          | _ => (err (); bogus))
                   | _ => (err (); bogus)
         end
   end

structure Exp =
   struct
      open Exp

      fun tuple es =
         if 1 = Vector.length es
            then node (Vector.sub (es, 0))
         else Record (Record.tuple es)

      val unit = tuple (Vector.new0 ())
   end

structure Dec =
   struct
      open Dec

      fun sequence (d1: t, d2: t): t =
         makeRegion (case (node d1, node d2) of
                        (SeqDec d1, SeqDec d2) => SeqDec (Vector.concat [d1, d2])
                      | (SeqDec d1, _) =>
                           SeqDec (Vector.concat [d1, Vector.new1 d2])
                      | (_, SeqDec d2) =>
                           SeqDec (Vector.concat [Vector.new1 d1, d2])
                      | _ => SeqDec (Vector.new2 (d1, d2)),
                     Region.append (region d1, region d2))
   end

structure Spec =
   struct
      open Spec

      (* Some of this mess is so that a sharing equation captures as
       * many specs as possible in its scope.
       *)
      fun seq (s: t, s': t): t =
         let
            fun reg s'' = makeRegion (s'', Region.append (region s, region s'))
         in
            case (node s, node s') of
               (Empty, _) => s'
             | (_, Empty) => s
             | (_, Seq (s1, s2)) => reg (Seq (seq (s, s1), s2))
             | (_, Sharing {spec, equations}) =>
                  reg (Sharing {spec = seq (s, spec), equations = equations})
             | _ => reg (Seq (s, s'))
         end

(*      val seq = Trace.trace2 ("Spec.seq", layout, layout, layout) seq *)
   end

fun consTopdec (d, dss) =
   case dss of
      [] => [[d]]
    | ds :: dss => (d :: ds) :: dss

type rule = Pat.t * Exp.t
type clause = {pats : Pat.t vector,
               resultType : Type.t option,
               body : Exp.t}
type clauses = clause vector
type eb = Con.t * EbRhs.t
type db = {tyvars: Tyvar.t vector,
           tycon: Tycon.t,
           cons: (Con.t * Type.t option) vector}

type strdesc = Strid.t * Sigexp.t

type wherespec = {tyvars: Tyvar.t vector,
                  longtycon: Longtycon.t,
                  ty: Type.t}

type typdesc =  {tyvars: Tyvar.t vector,
                 tycon: Tycon.t}

type valdesc = Var.t * Type.t

type exndesc = Con.t * Type.t option

type strbind = {name: Strid.t,
                def: Strexp.t,
                constraint: SigConst.t}

type sigbind = Sigid.t * Sigexp.t

type funbind = {name : Fctid.t,
                arg : FctArg.t,
                result : SigConst.t,
                body : Strexp.t}

type vb = {pat: Pat.t,
           exp: Exp.t}

type rvb = {pat: Pat.t,
            match: Match.t}

fun ensureNonqualified (ss: Symbol.t list, r: Region.t): Symbol.t * Region.t =
   case ss of
      [s] => (s, r)
    | _ => (error (r, "expected nonqualified id")
            ; (Symbol.bogus, r))

fun cons1 (x, (l, r, y)) = (x :: l, r, y)

fun augment (id, sigexp, (wherespecs, right, binds)) =
   (id, Sigexp.wheree (sigexp, Vector.fromList wherespecs,
                       Region.extendRight (Sigexp.region sigexp, right)))
   :: binds

fun 'a augment1 ((strexp: Strexp.t,
                  makesigconst: Sigexp.t -> SigConst.t,
                  sigexp: Sigexp.t),
                 (wherespecs: wherespec list,
                  right: SourcePos.t,
                  z: 'a)): Strexp.t * 'a =
   (Strexp.makeRegion
    (Strexp.Constrained
     (strexp, makesigconst (Sigexp.wheree
                            (sigexp, Vector.fromList wherespecs,
                             Region.extendRight (Sigexp.region sigexp, right)))),
     Region.extendRight (Strexp.region strexp, right)),
    z)

type 'a whereAnd = wherespec list * SourcePos.t * 'a list

  %%
  %term
      CHAR of IntInf.t
    | INT of {digits: string,
              negate: bool,
              radix: StringCvt.radix}
    | LONGID of string
    | REAL of string
    | STRING of IntInf.t vector
    | TYVAR of string
    | WORD of {digits: string,
               radix: StringCvt.radix}
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
         aexp of Exp.node
       | andspecs of wherespec list
       | apat of Pat.t
       | apat' of Pat.t
       | apatnode of Pat.node
       | apats of Pat.t list
       | app_exp of Exp.t list
       | app_exp1 of Exp.t list
       | arg_fct of Strexp.t
       | ieattributes of PrimKind.ImportExportAttribute.t list
       | clause of clause
       | clauses of clause list
       | clausesTop of clauses
       | commapats of Pat.t list
       | con of Con.t
       | const of Const.t
       | const' of Const.node
       | constr of Con.t * Type.t option
       | constraint of Type.t option
       | constrs of (Con.t * Type.t option) list
       | constOrBool of Const.t
       | datBind of DatBind.t
       | datBindNoWithtype of DatBind.t
       | datatypeRhs of DatatypeRhs.t
       | datatypeRhsNoWithtype of DatatypeRhs.t
       | datatypeRhsnode of DatatypeRhs.node
       | datatypeRhsnodeNoWithtype of DatatypeRhs.node
       | db of db
       | dbs of db vector
       | dbs' of db list
       | dec of Dec.t
       | decnode of Dec.node
       | decnolocal of Dec.node
       | decs of Dec.t
       | decsnode of Dec.node
       | digit of int
       | eb of eb
       | ebrhs of EbRhs.t
       | ebrhsnode of EbRhs.node
       | ebs of eb list
       | elabel of (Field.t * Exp.t)
       | elabels of (Field.t * Exp.t) list
       | exndesc of exndesc
       | exndescs of exndesc list
       | exp of Exp.t
       | exp_2c of Exp.t list
       | exp_list of Exp.t list
       | exp_ps of Exp.t list
       | expnode of Exp.node
       | expsAndTopdecs of Topdec.t list list
       | fctarg of FctArg.node
       | fctid of Fctid.t
       | field of Field.t
       | fixity of Fixity.t
       | funbinds of funbind list
       | funbinds' of Strexp.t * funbind list
       | funbinds'1 of funbind whereAnd
       | funbinds'1' of funbind whereAnd
       | funbinds'2 of funbind list
       | funs of clauses list
       | id of Symbol.t * Region.t
       | idEqual of Symbol.t * Region.t
       | idNoAsterisk of Symbol.t * Region.t
       | int of IntInf.t
       | longcon of Longcon.t
       | longid of Symbol.t list * Region.t
       | longidEqual of Symbol.t list * Region.t
       | longidNoAsterisk of Symbol.t list * Region.t
       | longstrid of Longstrid.t
       | longstrideqns of Longstrid.t list
       | longstrids of Longstrid.t list
       | longtycon of Longtycon.t
       | longtyconeqns of Longtycon.t list
       | longvidands of Longvid.t list
       | longvid of Longvid.t
       | longvidNoEqual of Longvid.t
       | match of Match.t
       | opaspat of Pat.t option
       | opcon of Con.t
       | ot_list of Exp.t list
       | pat of Pat.t
       | pat_2c of Pat.t list
       | patitem of (Field.t * Pat.Item.t)
       | patitems of ((Field.t * Pat.Item.t) list * bool)
       | patnode of Pat.node
       | pats of Pat.t list
       | priority of Priority.t
       | program of Program.t
       | repl of DatatypeRhs.node
       | rule of rule
       | rules of rule list
       | rvalbind of rvb list
       | rvalbindRest of rvb list
       | sdec of Dec.t
       | sdecs of Dec.t
       | sdecsPlus of Dec.t
       | sharespec of Equation.node
       | sigbinds of sigbind list
       | sigbinds' of sigbind whereAnd
       | sigbinds'' of sigbind whereAnd
       | sigconst of SigConst.t
       | sigexp of Sigexp.t
       | sigexp' of Sigexp.t
       | sigexp'node of Sigexp.node
       | sigexpnode of Sigexp.node
       | sigid of Sigid.t
       | sigids of Sigid.t list
       | spec of Spec.t
       | specnode of Spec.node
       | specs of Spec.t
       | strbinds of strbind list
       | strbinds' of Strexp.t * strbind list
       | strbinds'1 of strbind whereAnd
       | strbinds'1' of strbind whereAnd
       | strbinds'2 of strbind list
       | strdec of Strdec.t
       | strdecnode of Strdec.node
       | strdecs of Strdec.t
       | strdecsnode of Strdec.node
       | strdescs of strdesc list
       | strdescs' of strdesc whereAnd
       | strdescs'' of strdesc whereAnd
       | strexp of Strexp.t
       | strexp1 of Strexp.t * (Sigexp.t -> SigConst.t) * Sigexp.t
       | strexp2 of Strexp.t
       | strexp2node of Strexp.node
       | strexpnode of Strexp.node
       | strid of Strid.t
       | string of string
       | symattributes of PrimKind.SymbolAttribute.t list
       | tlabel of (Field.t * Type.t)
       | tlabels  of (Field.t * Type.t) list
       | topdec of Topdec.t
       | topdecnode of Topdec.node
       | topdecs of Topdec.t list list
       | tuple_ty of Type.t list
       | ty of Type.t
       | ty' of Type.t
       | ty'node of Type.node
       | ty0_pc of Type.t list
       | ty1 of Type.t
       | tyOpt of Type.t option
       | tycon of Tycon.t
       | tynode of Type.node
       | typBind of TypBind.t
       | typBind' of {def: Type.t,
                      tycon: Tycon.t,
                      tyvars: Tyvar.t vector} list
       | typBind'' of {def: Type.t,
                       tycon: Tycon.t,
                       tyvars: Tyvar.t vector} list
       | typdesc of typdesc
       | typdescs of typdesc list
       | tyvar of Tyvar.t
       | tyvar_pc of Tyvar.t list
       | tyvars of Tyvar.t vector
       | tyvarseq of Tyvar.t vector
       | valbind of vb list * rvb list
       | valbindRest of vb list * rvb list
       | valbindTop of vb vector * rvb vector
       | valdesc of valdesc
       | valdescs of valdesc list
       | var of Var.t
       | vid of Vid.t
       | vidNoEqual of Vid.t
       | vids of Vid.t list
       | wherespec of wherespec
       | wherespecs of wherespec vector
       | wherespecs' of wherespec list
       | withtypes of TypBind.t
       | word of IntInf.t

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

%value CHAR (IntInf.fromInt (Char.ord #"a"))
%value INT ({digits = "0", negate = false, radix = StringCvt.DEC})
%value LONGID ("bogus")
%value REAL ("13.0")
%value STRING (Vector.fromList [])
%value TYVAR ("'a")
%value WORD ({digits = "0", radix = StringCvt.DEC})

%%

program: expsAndTopdecs
                ( join (wrap ExpAndTopdecs
                             expsAndTopdecsleft
                             expsAndTopdecsright
                       )
                       expsAndTopdecs
                )

expsAndTopdecs:
    exp SEMICOLON expsAndTopdecs
                ( exp :: expsAndTopdecs)
  | topdecs
                ( topdecs )

topdecs:
                ( nil )
  | topdec topdecs
                ( topdec :: topdecs )
  | SEMICOLON expsAndTopdecs
                ( expsAndTopdecs )

topdec : topdecnode ( topdecnode )

topdecnode:
     strdec
                ( strdec )
   | SIGNATURE sigbinds
                ( join (wrap Topdec_Sig
                             SIGNATUREleft
                             sigbindsright
                       )
                       sigbinds
                )
   | FUNCTOR funbinds
                ( join (wrap Topdec_Fun
                             FUNCTORleft
                             funbindsright
                       )
                       funbinds
                )

(*---------------------------------------------------*)
(*                    Structures                     *)
(*---------------------------------------------------*)

strdecs : strdecsnode
                ( join (wrap Strdecs
                             strdecsnodeleft
                             strdecsnoderight
                       )
                       strdecsnode
                )

strdecsnode :
                ( nil )
   | SEMICOLON strdecs
                ( strdecs )
   | strdec strdecs
                ( strdec :: strdecs )

strdec : strdecnode
                ( strdecnode )

strdecnode:
     STRUCTURE strbinds
                ( join (wrap Topdec_Str
                             STRUCTUREleft
                             strbindsright
                       )
                       strbinds
                )
   | LOCAL strdecs IN strdecs END
                ( join (wrap Strdec_Local
                             LOCALleft
                             ENDright
                       )
                       [strdecs1, strdecs2]
                )
   | decnolocal ( decnolocal )

strbinds : strid sigconst EQUALOP strbinds'
           ( let
              val (strexp, strbinds) = strbinds'
            in
              (join (wrap Strbind
                          stridleft
                          (right strexp)
                    )
                    [strid, strexp, sigconst]
              ) :: strbinds
            end

strbinds' : strexp1 strbinds'1
            ( let
               val (strexp, sigcon, sigexp) = strexp1
               val (wherespecs, strbinds) = strbinds'1
               val sigexp' =
                   case wherespecs of
                     nil => sigexp
                   |     =>
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
                           [sigexp]
                     ],
                strbinds)
             end)
          | strexp2 strbinds'2
            ( (strexp2, strbinds'2) )

strbinds'1 : strbinds'2
               ( (nil, strbinds'2) )
           | WHERE wherespec strbinds'1'
             ( let
                val (wherespecs, strbinds) = strbinds'1'
              in
                (wherespec :: wherespecs, strbinds)
              end )

strbinds'1' : strbinds'1
                ( strbinds'1 )
            | AND wherespec strbinds'1'
              ( let
                 val (wherespecs, strbinds) = strbinds'1
               in
                 (wherespec :: wherespecs, strbinds)
               end

strbinds'2 :     ( nil )
           | AND strbinds
                 ( strbinds )

strexp : strexpnode
           ( strexpnode )

strexpnode
  : strexp1
      (let
         val (strexp, sigcon, sigexp) = strexp1
       in
         join (wrap Strexp_Con strexp1left strexp1right)
              [strexp, join (wrap (Sigcon sigcon) (left sigexp) (right sigexp))
                            [sigexp]
              ]
       end)
  | strexp1 wherespecs
      (let
         val (strexp, sigcon, sigexp) = strexp1
       in
         join (wrap Strexp_Con strexp1left strexp1right)
              [strexp,
               join (wrap (Sigcon sigcon) (left sigexp) wherespecsright)
                    [join (wrap Sigexp_Where (left sigexp) wherespecsright)
                          [sigexp, wherespecs]
                    ]
              ]
       end)
  | strexp2node
      (strexp2node)

strexp1
  : strexp COLON sigexp'
           ((strexp, Sigcon.Transparent, sigexp'))
  | strexp COLONGT sigexp'
           ((strexp, Sigcon.Opaque, sigexp'))

strexp2
  : strexp2node
      (let
         val (node, children) = strexp2node
       in
         join (wrap node strexp2nodeleft strexp2noderight)
              children
       end)

strexp2node
  : longid
      ((longid, nil))
  | STRUCT strdecs END
      ((Strexp_Struct, [strdecs]))
  | longid arg_fct
      ((Strexp_Fun, [longid, arg_fct]))
  | LET strdecs IN strexp END
      ((Strexp_Let, [strdecs, strexp]))

arg_fct
  : LPAREN strexp RPAREN
      (strexp)
  | LPAREN strdecs RPAREN
      (strdecs)

(*---------------------------------------------------*)
(*                    Signatures                     *)
(*---------------------------------------------------*)

sigexp
  : sigexp'
      (sigexp')
  | sigexp' wherespecs
      (join (wrap Sigexp_Where sigexp'left wherespecsright)
            [sigexp', wherespecs]
      )

wherespecs
  : wherespecs'
      (join (wrap Wherespecs wherespecs'left wherespecs'right)
            [wherespecs']
      )

wherespecs'
  : WHERE wherespec
      ([wherespec])
  | WHERE wherespec wherespecs'
      (wherespec :: wherespecs')
  | WHERE wherespec andspecs
      (wherespec :: andspecs)

andspecs
  : AND wherespec
      ([wherespec])
  | AND wherespec andspecs
      (wherespec :: andspecs)
  | AND wherespec wherespecs'
      (wherespec :: wherespecs')

sigbinds: sigid EQUALOP sigexp' sigbinds'
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
         join (wrap Sigbind sigidleft (right sigexp))
              [sigid, sigexp]
       end

sigexp'
  : sigexp'node
      (sigexp'node)

sigexp'node
  : sigid
      (sigid)
  | SIG specs END
      (join (wrap Sigexp_Spec SIGleft ENDright)
            specs
      )

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

sigbinds''
  : sigbinds'
      (sigbinds')
  | AND wherespec sigbinds''
      (let
         val (wherespecs, sigbinds) = sigbinds''
       in
         (wherespec :: wherespecs, sigbinds)
       end)

wherespec
  : TYPE tyvars longtycon EQUALOP ty
      (join (wrap Wherespec TYPEleft tyright)
            [tyvars, longtycon, ty]
      )

sigconst :
      (join (wrap (Sigcon Sigcon.None) dummypos dummypos) nil)
  | COLON sigexp
      (join (wrap (sigcon Sigcon.Transparent) sigexpleft sigexpright)
            [sigexp]
      )
  | COLONGT sigexp
      (join (wrap (sigcon Sigcon.Opaque) sigexpleft sigexpright)
            [sigexp]
      )

specs :
      (nil)
  | SEMICOLON specs
      (specs)
  | spec specs
      (spec :: specs)

spec
  : specnode
      (let
         val (node, children) = specnode
       in
         join (wrap node specnodeleft specnoderight)
              children
       end)

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
      ((Spec_Datatype, nil))
  | EXCEPTION exndescs
      ((Spec_Exception, exndescs))
  | STRUCTURE strdescs
      ((Spec_Structure, strdescs))
  | INCLUDE sigexp
      ((Spec_Include, [sigexp]))
  | INCLUDE sigid sigids (* p. 59 *)
      ((Spec_IncludeSigids, sigid :: sigids))
  | sharespec
      (sharespec)

sharespec
  : SHARING TYPE longtyconeqns
      ((Spec_Sharing, longtyconeqns))
  | SHARING longstrideqns
      ((Spec_SharingStructure, longstrideqns))

longstrideqns
  : longstrid EQUALOP longstrid
      ([longstrid1, longstrid2])
  | longstrid EQUALOP longstrideqns
      (longstrid :: longstrideqns)

longtyconeqns
  : longtycon EQUALOP longtycon
      ([longtycon1, longtycon2])
  | longtycon EQUALOP longtyconeqns
      (longtycon :: longtyconeqns)

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
         join (wrap Strdesc stridleft (right sigexp))
              [strid, sigexp]
       end)

strdescs' :
      ((nil, nil))
  | AND strdescs
      ((nil, strdescs))
  | WHERE wherespec strdescs''
      (let
         val (wherespec, strdescs) = strdescs''
       in
         (wherespec :: wherespecs, strdescs)
       end)

strdescs''
  : strdescs'
      (strdescs')
  | AND wherespec strdescs''
      (let
         val (wherespec, strdescs) = strdescs''
       in
         (wherespec :: wherespecs, strdescs)
       end)

typdescs
  : typdesc
      ([typdesc])
  | typdesc AND typdescs
      (typdesc :: typdescs)

typdesc
  : tyvars tycon
      (join (wrap Tydesc tyvarsleft tyconright)
            [tyvars, tycon]
      )

valdescs
  : valdesc
      ([valdesc])
  | valdesc AND valdescs
      (valdesc :: valdescs)

valdesc
  : var COLON ty
      (join (wrap Valdesc varleft tyright)
            [var, ty]
      )

exndescs
  : exndesc
      ([exndesc])
  | exndesc AND exndescs
      (exndesc :: exndescs)

exndesc
  : con tyOpt
      (join (wrap Exndesc conleft tyOptright)
            [con,
             join (wrap MaybeTy dummypos dummypos)
                  (case tyOpt of
                     SOME ty => [ty]
                   | NONE    => []
                  )
            ]
      )

tyOpt :
      (NONE)
  | OF ty
      (SOME ty)

(*---------------------------------------------------*)
(*                     Functors                      *)
(*---------------------------------------------------*)

funbinds
  : fctid LPAREN fctarg RPAREN sigconst EQUALOP funbinds'
      (let
         val (strexp, funbinds) = funbinds'
       in
         (join (wrap Funbind fctidleft (right strexp))
               [fctid, fctarg, strexp, sigconst]
         ) :: funbinds
       end)

funbinds'
  : strexp1 funbinds'1
      (let
         val (strexp, sigcon, sigexp) = strexp1
         val (wherespecs, funbinds) = funbinds'1
         val sigexp' =
             case wherespecs of
               nil => sigexp
             |     =>
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
                     [sigexp]
               ],
          funbinds)
       end
  | strexp2 funbinds'2
      ((strexp2, funbinds'2))

funbinds'1
  : funbinds'2
      ((nil, funbinds'2))
  | WHERE wherespec funbinds'1'
      (let
         val (wherespecs, funbinds) = funbinds'1'
       in
         (wherespec :: wherespecs, funbinds)
       end)

funbinds'2 :
      (nil)
  | AND funbinds
      (funbinds)

funbinds'1'
  : funbinds'1
      (funbinds'1)
  | AND wherespec funbinds'1'
      (let
         val (wherespecs, funbinds) = funbinds'1'
       in
         (wherespec :: wherespecs, funbinds)
       end)

fctarg
  : strid COLON sigexp
      (join (wrap Funarg_Structure stridleft sigexpright)
            [strid, sigexp]
      )
  | specs
      (join (wrap Funarg_Spec specsleft specsright)
            specs
      )

(*---------------------------------------------------*)
(*                   Declarations                    *)
(*---------------------------------------------------*)

decs :                  (Dec.makeRegion' (Dec.SeqDec (Vector.new0 ()),
                                          defaultPos, defaultPos))
     | dec decs         (Dec.sequence (dec,decs))
     | SEMICOLON decs   (decs)

dec : decnode (Dec.makeRegion' (decnode, decnodeleft, decnoderight))

decnode : decnolocal              (decnolocal)
        | LOCAL decs IN decs END  (Dec.Local (decs1,decs2))

decnolocal
        : VAL valbindTop          (Dec.Val {tyvars = Vector.new0 (),
                                            vbs = #1 valbindTop,
                                            rvbs = #2 valbindTop})
        | VAL tyvarseq valbindTop  (Dec.Val {tyvars = tyvarseq,
                                             vbs = #1 valbindTop,
                                             rvbs = #2 valbindTop})
        | FUN funs              (Dec.Fun (Vector.new0 (), Vector.fromList funs))
        | FUN tyvarseq funs     (Dec.Fun (tyvarseq, Vector.fromList funs))
        | TYPE typBind          (Dec.Type typBind)
        | DATATYPE datatypeRhs  (Dec.Datatype datatypeRhs)
        | ABSTYPE datBind WITH decs END   (Dec.Abstype {datBind = datBind,
                                                        body = decs})
        | EXCEPTION ebs
          (Dec.Exception (Vector.fromList ebs))
        | OPEN longstrids       (Dec.Open (Vector.fromList longstrids))
        | fixity vids           (Dec.Fix {fixity = fixity,
                                          ops = Vector.fromList vids})
        | OVERLOAD priority var COLON ty AS longvidands
                                (Dec.Overload (priority, 
                                               var,
                                               Vector.new0 (),
                                               ty,
                                               Vector.fromList longvidands))

valbindTop : valbind (let
                         val (vbs, rvbs) = valbind
                      in
                         (Vector.fromList vbs,
                          Vector.fromList rvbs)
                      end)

valbind : pat EQUALOP exp valbindRest
          (let
              val (vbs, rvbs) = valbindRest
           in
              ({pat = pat, exp = exp} :: vbs,
               rvbs)
           end)
        | REC rvalbind                 (([], rvalbind))


valbindRest :                          (([], []))
            | AND valbind              (valbind)

rvalbind : REC rvalbind                (rvalbind)
         | pat EQUALOP FN match rvalbindRest
            ({pat = pat, match = match} :: rvalbindRest)

rvalbindRest :               ([])
             | AND rvalbind  (rvalbind)

constraint :                    (NONE)
           | COLON ty           (SOME ty)

funs    : clausesTop               ([clausesTop])
        | clausesTop AND funs      (clausesTop :: funs)

clausesTop: clauses (Vector.fromList clauses)

clauses : clause                ([clause])
        | clause BAR clauses    (clause :: clauses)

clause  : apats constraint EQUALOP exp  ({pats = Vector.fromList apats,
                                          resultType = constraint,
                                          body = exp})

typBind : typBind'
          (let
              val typBind = Vector.fromList typBind'
              val b =
                 TypBind.makeRegion'
                 (TypBind.T typBind, typBind'left, typBind'right)
           in
              b
           end)

typBind' : tyvars tycon EQUALOP ty typBind''
           ({def = ty, tycon = tycon, tyvars = tyvars} :: typBind'')

typBind'' :               ([])
          | AND typBind'  (typBind')


tyvars  : tyvarseq (tyvarseq)
        |          (Vector.new0 ())

tyvarseq: tyvar                   (Vector.new1 tyvar)
        | LPAREN tyvar_pc RPAREN
          (let
              val v = Vector.fromList tyvar_pc
              val () =
                 reportDuplicates
                 (v, {equals = Tyvar.sameName,
                      layout = Tyvar.layout,
                      name = "type variable",
                      region = Tyvar.region,
                      term = fn () => Layout.tuple (Vector.toListMap
                                                    (v, Tyvar.layout))})
           in
              v
           end)

tyvar_pc: tyvar                ([tyvar])
        | tyvar COMMA tyvar_pc (tyvar :: tyvar_pc)

constrs : constr                ([constr])
        | constr BAR constrs    (constr :: constrs)

constr  : opcon         (opcon, NONE)
        | opcon OF ty   (opcon, SOME ty)

opcon   : con           (con)
        | OP con        (con)

ebs     : eb              ([eb])
        | eb AND ebs      (eb::ebs)

eb      : opcon ebrhs     (Con.ensureRedefine opcon; (opcon, ebrhs))

ebrhs : ebrhsnode (EbRhs.makeRegion' (ebrhsnode,
                                     ebrhsnodeleft, ebrhsnoderight))

ebrhsnode   :                    (EbRhs.Gen NONE)
            | OF ty              (EbRhs.Gen (SOME ty))
            | EQUALOP longcon    (EbRhs.Def longcon)
            | EQUALOP OP longcon (EbRhs.Def longcon)

fixity  : INFIX                 (Fixity.Infix NONE)
        | INFIX digit           (Fixity.Infix (SOME digit))
        | INFIXR                (Fixity.Infixr NONE)
        | INFIXR digit          (Fixity.Infixr (SOME digit))
        | NONFIX                (Fixity.Nonfix)

priority :                      (Priority.T NONE)
         | digit                (Priority.T (SOME digit))

int : INT
   (let
       val {digits, negate, radix} = INT
    in
       case StringCvt.scanString (fn r => IntInf.scan (radix, r)) digits of
          NONE => Error.bug "parser saw invalid int"
        | SOME i => if negate then ~ i else i
    end)

word : WORD
   (let
       val {digits, radix} = WORD
    in
       case StringCvt.scanString (fn r => IntInf.scan (radix, r)) digits of
          NONE => Error.bug "parser saw invalid word"
        | SOME i => i
    end)

digit : INT
   (let
       val {digits, negate, radix} = INT
    in
       if 1 = String.size digits andalso not negate andalso radix = StringCvt.DEC
          then valOf (Int.fromString digits)
       else let
               open Layout
               val _ = 
                  Control.error (reg (INTleft, INTright),
                                 str "invalid digit in infix declaration",
                                 empty)
            in
               0
            end
    end)

datatypeRhs
   : datatypeRhsnode
     (DatatypeRhs.makeRegion' (datatypeRhsnode,
                               datatypeRhsnodeleft, datatypeRhsnoderight))

datatypeRhsNoWithtype
   : datatypeRhsnodeNoWithtype
     (DatatypeRhs.makeRegion' (datatypeRhsnodeNoWithtype,
                               datatypeRhsnodeNoWithtypeleft,
                               datatypeRhsnodeNoWithtyperight))

datatypeRhsnode
   : repl              (repl)
   | datBind           (DatatypeRhs.DatBind datBind)

datatypeRhsnodeNoWithtype
   : repl               (repl)
   | datBindNoWithtype  (DatatypeRhs.DatBind datBindNoWithtype)

repl : tyvars tycon EQUALOP DATATYPE longtycon
       (if Vector.isEmpty tyvars
           then ()
        else error (reg (tyvarsleft, tyvarsright),
                    "nonempty tyvars in datatype repl")
        ; DatatypeRhs.Repl {lhs = tycon, rhs = longtycon})

datBind
   : dbs withtypes
     (DatBind.make (dbs, withtypes, dbsleft, withtypesright))

datBindNoWithtype
   : dbs
     (DatBind.make (dbs, TypBind.empty, dbsleft, dbsright))

dbs : dbs' (Vector.fromList dbs')

dbs'
   : db
     ([db])
   | db AND dbs'
     (db :: dbs')

db : tyvars tycon EQUALOP constrs
     ({cons = Vector.fromList constrs,
       tycon = tycon,
       tyvars = tyvars})

withtypes
   :
     (TypBind.empty)
   | WITHTYPE typBind
     (typBind)

longvidands : longvid  ([longvid])
            | longvid AND longvidands (longvid :: longvidands)

match : rules           (Match.makeRegion' (Match.T (Vector.fromList rules),
                                            rulesleft, rulesright))

rules : rule            ([rule])
      | rule BAR rules  (rule :: rules)

rule    : pat DARROW exp        ((pat,exp))

elabel  : field EQUALOP exp     (field,exp)

elabels : elabel COMMA elabels  (elabel :: elabels)
        | elabel                ([elabel])

exp_ps  : exp SEMICOLON exp     ([exp1, exp2])
        | exp SEMICOLON exp_ps  (exp :: exp_ps)

exp : expnode (Exp.makeRegion' (expnode, expnodeleft, expnoderight))

expnode : exp HANDLE match      (Exp.Handle (exp, match))
        | exp ORELSE exp        (Exp.Orelse (exp1, exp2))
        | exp ANDALSO exp       (Exp.Andalso (exp1, exp2))
        | exp COLON ty          (Exp.Constraint (exp, ty))
        | app_exp               (Exp.FlatApp (Vector.fromList app_exp))
        | FN match              (Exp.Fn match)
        | CASE exp OF match     (Exp.Case (exp, match))
        | WHILE exp DO exp      (Exp.While {test = exp1, expr = exp2})
        | IF exp THEN exp ELSE exp (Exp.If (exp1, exp2, exp3))
        | RAISE exp             (Exp.Raise exp)

app_exp : aexp app_exp1     (Exp.makeRegion' (aexp, aexpleft, aexpright)
                             :: app_exp1)
        | longvid app_exp1  (Exp.makeRegion' (Exp.Var {name = longvid,
                                                       fixop = Fixop.None},
                                             longvidleft, longvidright)
                             :: app_exp1)

app_exp1 :         ([])
         | app_exp (app_exp)

aexp    : OP longvid            (Exp.Var {name = longvid,
                                          fixop = Fixop.Op})
        | const                 (Exp.Const const)
        | HASH field            (Exp.Selector field)
        | LBRACE elabels RBRACE
          (Exp.Record (Record.fromVector (Vector.fromList elabels)))
        | LBRACE RBRACE         (Exp.unit)
        | LPAREN RPAREN         (Exp.unit)
        | LPAREN expnode RPAREN (expnode)
        | LPAREN exp_ps RPAREN  (Exp.Seq (Vector.fromList exp_ps))
        | LPAREN exp_2c RPAREN  (Exp.tuple (Vector.fromList exp_2c))
        | LBRACKET exp_list RBRACKET  (Exp.List (Vector.fromList exp_list))
        | LBRACKET RBRACKET           (Exp.List (Vector.new0 ()))
        | LET decs IN exp END   (Exp.Let (decs, exp))
        | LET decs IN exp_ps END
            (Exp.Let (decs, Exp.makeRegion' (Exp.Seq (Vector.fromList exp_ps),
                                             exp_psleft,
                                             exp_psright)))
        | ADDRESS string COLON ty SEMICOLON
          (Exp.Prim (PrimKind.Address {name = string,
                                       ty = ty}))
        | BUILD_CONST string COLON ty SEMICOLON
          (Exp.Prim (PrimKind.BuildConst {name = string, ty = ty}))
        | COMMAND_LINE_CONST string COLON ty EQUALOP constOrBool SEMICOLON
          (Exp.Prim (PrimKind.CommandLineConst {name = string,
                                                ty = ty,
                                                value = constOrBool}))
        | CONST string COLON ty SEMICOLON
          (Exp.Prim (PrimKind.Const {name = string, ty = ty}))
        | EXPORT string ieattributes COLON ty SEMICOLON
          (Exp.Prim (PrimKind.Export {attributes = ieattributes,
                                      name = string,
                                      ty = ty}))
        | IMPORT string ieattributes COLON ty SEMICOLON
          (Exp.Prim (PrimKind.Import {attributes = ieattributes,
                                      name = string,
                                      ty = ty}))
        | IMPORT ASTERISK ieattributes COLON ty SEMICOLON
          (Exp.Prim (PrimKind.IImport {attributes = ieattributes,
                                       ty = ty}))
        | PRIM string COLON ty SEMICOLON
          (Exp.Prim (PrimKind.Prim {name = string,
                                    ty = ty}))
        | SYMBOL string symattributes COLON ty SEMICOLON
          (Exp.Prim (PrimKind.Symbol {attributes = symattributes,
                                      name = string,
                                      ty = ty}))
        | SYMBOL ASTERISK COLON ty SEMICOLON
          (Exp.Prim (PrimKind.ISymbol {ty = ty}))

ieattributes
   :
     ([])
   | id ieattributes
     (let
         val id = Symbol.toString (#1 id)
      in
         case id of
            "cdecl" => PrimKind.ImportExportAttribute.Cdecl :: ieattributes
          | "stdcall" => PrimKind.ImportExportAttribute.Stdcall :: ieattributes
          | _ => (error (reg (idleft, idright), concat ["invalid attribute", id])
                  ; ieattributes)
      end)

symattributes
   :
     ([])
   | id symattributes
     (let
         val id = Symbol.toString (#1 id)
      in
         case id of
            "alloc" => PrimKind.SymbolAttribute.Alloc :: symattributes
          | _ => (error (reg (idleft, idright), concat ["invalid attribute", id])
                  ; symattributes)
      end)

exp_2c  : exp COMMA exp_2c      (exp :: exp_2c)
        | exp COMMA exp         ([exp1, exp2])

exp_list : exp                  ([exp])
         | exp COMMA exp_list   (exp :: exp_list)

(*---------------------------------------------------*)
(*                     Patterns                      *)
(*---------------------------------------------------*)

pat : patnode (Pat.makeRegion' (patnode, patnodeleft, patnoderight))

patnode : pat AS pat    (Pat.makeAs (pat1, pat2))
        | pat COLON ty  (Pat.Constraint (pat, ty))
        | apats         (Pat.FlatApp (Vector.fromList apats))

apats   : apat                  ([apat])
        | apat apats            (apat :: apats)

apat : apatnode (Pat.makeRegion' (apatnode, apatnodeleft, apatnoderight))

apatnode : longvidNoEqual        (Pat.Var {name = longvidNoEqual,
                                           fixop = Fixop.None})
        | OP longvid             (Pat.Var {name = longvid,
                                           fixop = Fixop.Op})
        | const
          (let
              val _ =
                 case Const.node const of
                    Const.Real r =>
                       let
                          open Layout
                       in
                          Control.error
                          (Const.region const,
                           seq [str "real constants are not allowed in patterns: ",
                                Const.layout const],
                           empty)
                       end
                    | _ => ()
           in
              Pat.Const const
           end)
        | WILD                   (Pat.Wild)
        | LPAREN pats RPAREN     (Pat.tuple (Vector.fromList pats))
        | LBRACKET pats RBRACKET (Pat.List (Vector.fromList pats))
        | LBRACE RBRACE          (Pat.unit)
        | LBRACE patitems RBRACE
          (let
              val (items, flexible) = patitems
           in
              Pat.Record {flexible = flexible,
                          items = Vector.fromList items}
           end)

pats: ([])
    | pat commapats (pat :: commapats)

commapats : ([])
          | COMMA pat commapats (pat :: commapats)

patitems : patitem COMMA patitems  (let val (items, f) = patitems
                                    in (patitem :: items, f)
                                    end)
         | patitem              ([patitem], false)
         | DOTDOTDOT            ([], true)

patitem
   : field EQUALOP pat
     ((field, Pat.Item.Field pat))
   | vidNoEqual constraint opaspat
     (Field.Symbol (Vid.toSymbol vidNoEqual),
      Pat.Item.Vid (vidNoEqual, constraint, opaspat))

opaspat :         (NONE)
        | AS pat  (SOME pat)

(*---------------------------------------------------*)
(*                       Types                       *)
(*---------------------------------------------------*)

ty : tynode (Type.makeRegion' (tynode, tynodeleft, tynoderight))

tynode  : tuple_ty      (Type.tuple (Vector.fromList tuple_ty))
        | ty ARROW ty   (Type.arrow (ty1, ty2))
        | ty'node       (ty'node)

ty' : ty'node (Type.makeRegion' (ty'node, ty'nodeleft, ty'noderight))

ty'node : tyvar                           (Type.Var tyvar)
        | LBRACE tlabels RBRACE
          (Type.Record (Srecord.fromVector (Vector.fromList tlabels)))
        | LBRACE RBRACE                   (Type.unit)
        | LPAREN ty0_pc RPAREN longtycon  (Type.Con (longtycon,
                                                     Vector.fromList ty0_pc))
        | LPAREN ty RPAREN                (Type.node ty)
        | ty' longtycon                   (Type.Con (longtycon,
                                                     Vector.new1 ty'))
        | longtycon                       (Type.Con (longtycon,
                                                     Vector.new0 ()))

tlabel  : field COLON ty        (field, ty)

tlabels : tlabel COMMA tlabels  (tlabel :: tlabels)
        | tlabel                ([tlabel])

tuple_ty : ty' ASTERISK tuple_ty        (ty' :: tuple_ty)
         | ty' ASTERISK ty'             ([ty'1, ty'2])

ty0_pc  : ty COMMA ty           ([ty1, ty2])
        | ty COMMA ty0_pc       (ty :: ty0_pc)

(*---------------------------------------------------*)
(*                       Atoms                       *)
(*---------------------------------------------------*)

constOrBool
   : const (const)
   | id (let
            fun ok b = Const.makeRegion (Const.Bool b, reg (idleft, idright))
         in            
            case Symbol.toString (#1 id) of
               "false" => ok false
             | "true" => ok true
             | s => (error (#2 id, concat ["unknown boolean constant: ", s])
                     ; ok false)
         end)

const   : const'                (Const.makeRegion
                                 (const', reg (const'left, const'right)))

const'  : int                   (Const.Int int)
        | word                  (Const.Word word)
        | REAL                  (Const.Real REAL)
        | STRING                (Const.String STRING)
        | CHAR                  (Const.Char CHAR)

string : STRING  (CharVector.tabulate
                  (Vector.length STRING, fn i =>
                   Char.fromInt (Int.fromIntInf (Vector.sub (STRING, i)))))

idNoAsterisk : longidNoAsterisk (ensureNonqualified longidNoAsterisk)

id : idNoAsterisk  (idNoAsterisk)
   | ASTERISK      ((Symbol.asterisk, reg (ASTERISKleft, ASTERISKright)))

idEqual : id      (id)
        | EQUALOP ((Symbol.equal, reg (EQUALOPleft, EQUALOPright)))

longid
   : longidNoAsterisk (longidNoAsterisk)
   | ASTERISK  (([Symbol.asterisk], reg (ASTERISKleft, ASTERISKright)))

longidNoAsterisk
   : LONGID
     (let
         val syms = List.map (String.split (LONGID, #"."), Symbol.fromString)
      in
         (syms, reg (LONGIDleft, LONGIDright))
      end)

longidEqual : longid   (longid)
            | EQUALOP  (([Symbol.equal], reg (EQUALOPleft, EQUALOPright)))

vid : idEqual                  (Vid.fromSymbol idEqual)
vidNoEqual : id                (Vid.fromSymbol id)
vids : vid                     ([vid])
     | vid vids                (vid::vids)
var : idEqual                  (Var.fromSymbol idEqual)
con : id                       (Con.fromSymbol id)
tycon : idNoAsterisk           (Tycon.fromSymbol idNoAsterisk)
tyvar : TYVAR                  (Tyvar.newString (TYVAR, {left = TYVARleft,
                                                         right = TYVARright}))
field : id                     (Field.Symbol (#1 id))
      | int                    (let
                                   val int =
                                      IntInf.toInt int
                                      handle Exn.Overflow =>
                                         (error (reg (intleft, intright),
                                                 "field too huge")
                                          ; 0)
                                in
                                   Field.Int
                                   (if int <= 0
                                       then (error (reg (intleft, intright),
                                                    "nonpositive field")
                                             ; ~1)
                                    else
                                       int - 1)
                                end) (* int - 1 because fields are 0-based *)

strid : id                     (Strid.fromSymbol id)
sigid : id                     (Sigid.fromSymbol id)
sigids : sigid                 ([sigid])
       | sigid sigids          (sigid :: sigids)
fctid : id                     (Fctid.fromSymbol id)

longtycon : longidNoAsterisk (Longtycon.fromSymbols longidNoAsterisk)
longvid : longidEqual      (Longvid.fromSymbols longidEqual)
longvidNoEqual : longid    (Longvid.fromSymbols longid)
longcon : longid           (Longcon.fromSymbols longid)
longstrid : longid         (Longstrid.fromSymbols longid)

longstrids : longstrid             ([longstrid])
           | longstrid longstrids  (longstrid :: longstrids)

