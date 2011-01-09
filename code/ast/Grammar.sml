structure Grammar =
struct
fun die s = Crash.die "Grammar" s
type ident = (Ident.t, int) Wrap.t
type variable = (Variable.t, int) Wrap.t

(*
 Topdec = Strdec U Sigdec U Fundec U Dec U Exp
 Strdec' = Strdec U Dec U Exp
 *)

datatype ('ident, 'var) node =
         (* Rules *)
         Rule_Rules (* Rule_Type list *)

       (* Rule_Type *)
       | Rule_Type_Clauses of string (* [Rule_Scheme, Rule_Clauses] *)
       | Rule_Type_Expressions of string (* who knows? *)

       (* Rule_Scheme *)
       | Rule_Scheme (* [Rule_Clauses, Cstrns] *)

       (* Rule_Clauses *)
       | Rule_Clauses (* Rule_Clause list *)

       (* Rule_Clause *)
       | Rule_Clause (*  *)

       (* Rule_Cstrns *)
       | Rule_Cstrns (* Rule_Cstrn_Rel list *)

       (* Rule_Cstrn_Rel *)
       | Rule_Cstrn_Rel of string (* Meta_Pattern list *)

       (* Rule_Transformer *)
       | Rule_Trans of string (* [spat] *)

       (* Rule_Meta_Pat *)
       | Rule_Meta_Pat of string (* spat list | [] *)

       (* Rule_Self *)
       | Rule_Self (* [sexp] *)


       (****************************)
       (* Normal SML types follows *)
       (****************************)

       | Topdecs (* Topdec list *)

       (* Strdecs *)
       | Strdecs (* Strdec' list *)
       (* Strdec *)
       | Strdec_Str   (* Strbind list *)
       | Strdec_Local (* [Strdecs, Strdecs] *)

       (* Sigdec *)
       | Sigdec_Sig (* Sigbind list *)

       (* Fundec *)
       | Fundec_Fun (* Funbind list *)

       (* Strbind *)
       | Strbind of 'ident (* [Strexp, Sigcon] *)

       (* Sigbind *)
       | Sigbind of 'ident (* [Sigexp] *)

       (* Funbind *)
       | Funbind of 'ident (* [Funarg, Strexp, Sigcon] *)

       (* Funarg *)
       | Funarg_Structure of 'ident (* [Sigexp] *)
       | Funarg_Spec (* Spec list *)

       (* Strexp *)
       | Strexp_Struct (* Strdec' list *)
       | Strexp_Let (* [Strdecs, Strexp] *)
       | Strexp_Con (* [Strexp, Sigcon] *)
       | Strexp_Fun of 'ident (* [Strexp | Strdecs] *)
       | Strexp_Var of 'ident

       (* Sigcon *)
       | Sigcon of Sigcon.t (* [Sigexp] | [] *)

       (* Sigexp *)
       | Sigexp_Where (* [Sigexp, Wherespecs] *)
       | Sigexp_Spec (* Spec list *)
       | Sigexp_Var of 'ident

       (* Wherespecs *)
       | Wherespecs (* Wherespec list *)
       (* Wherespec *)
       | Wherespec of 'ident list * 'ident (* [Ty] *)

       (* Spec *)
       | Spec_Val (* Valdesc list *)
       | Spec_Type (* Tydesc list *)
       | Spec_Typedef (* Tybind list *)
       | Spec_EqType (* Tydesc list *)
       | Spec_Datatype (* Datdesc list *)
       | Spec_Replication of 'ident * 'ident (* [] *)
       | Spec_Exception (* Exndesc list *)
       | Spec_Structure (* Strdesc list *)
       | Spec_Include (* [Sigexp] *)
       | Spec_IncludeSigids of 'ident list
       | Spec_Sharing of 'ident list
       | Spec_SharingStructure of 'ident list

       (* Strdesc *)
       | Strdesc of 'ident (* [Sigexp] *)

       (* Tydesc *)
       | Tydesc of 'ident list * 'ident

       (* Valdesc *)
       | Valdesc of 'ident (* [Ty] *)

       (* Exndesc *)
       | Exndesc of 'ident (* [MaybeTy] *)

       (* Datatypes *)
       | Datatypes (* Datatype list *)
       (* Datatype *)
       | Datatype of 'ident list * 'ident (* Constructor list *)
       (* Constructor *)
       | Constructor of 'var (* [MaybeTy] *)
       (* Replication *)
       | Replication of 'var * 'var

       (* MaybeTy *)
       | MaybeTy (* [Ty] | [] *)

       (* Decs *)
       | Decs (* Dec list *)
       (* Dec *)
       | Dec_Local (* [Decs, Decs] *)
       | Dec_Val of 'ident list (* Valbind list *)
       | Dec_Fun of 'ident list (* Match list (Clause) *)
       | Dec_Type (* Tybind list *)
       | Dec_Datatype (* [Datatypes, Withtypes] *)
       | Dec_Replication of 'ident * 'ident (* [] *)
       | Dec_Abstype (* [Datbinds, Withtypes, Decs] *)
       | Dec_Exception (* (Constructor | Replication) list *)
       | Dec_Open of 'ident list
       | Dec_Fix of Fixity.t * 'ident list
       | Dec_Overload of int option * 'var * 'ident list (* [Ty] *)

       (* Valbind *)
       | Valbind_Plain (* [Pat, Exp] *)
       | Valbind_Rec (* [Pat, Match] *)

       (* Match *)
       | Match (* (Clause | Rule) list *)
       (* Clause *)
       | Clause of 'var (* [Pats, MaybeTy, Exp] *)
       | FlatClause (* [Pats, MaybeTy, Exp] *)
       | Rule (* [Pat, Exp] *)

       (* Datbinds *)
       | Datbinds (* Datatype list *)

       (* Withtypes *)
       | Withtypes (* Tybind list *)
       (* Tybind *)
       | Tybind of 'ident list * 'ident (* [Ty] *)

       (* Exps *)
       | Exps (* Exp list *)
       (* Exp *)
       | Exp_Handle (* [Exp, Match] *)
       | Exp_Orelse (* [Exp, Exp] *)
       | Exp_Andalso (* [Exp, Exp] *)
       | Exp_Typed (* [Exp, Ty] *)
       | Exp_App (* [Exp, Exp] *)
       | Exp_FlatApp (* Exp list *)
       | Exp_Fn (* [Match] *)
       | Exp_Case (* [Exp, Match] *)
       | Exp_While (* [Exp, Exp] *)
       | Exp_If (* [Exp, Exp, Exp] *)
       | Exp_Raise (* [Exp] *)
       | Exp_Var of 'var
       | Exp_SCon of SCon.t
       | Exp_Selector of 'ident
       | Exp_Record (* Label list *)
       | Exp_Unit
       | Exp_Par (* [Exp] *)
       | Exp_Seq (* Exp list *)
       | Exp_Tuple (* Exp list *)
       | Exp_List (* Exp list *)
       | Exp_Let (* [Decs, Exp] *)
       | Exp_LetSeq (* [Decs, Exps] *)

       (* Label *)
       | Label_Plain of 'ident (* [Exp | Pat | Ty] *)
       | Label_Short of 'var (* [MaybeTy, MaybePat] *)

       (* MaybePat *)
       | MaybePat (* [Pat] | [] *)
       (* Pats *)
       | Pats (* Pat list *)
       (* Pat *)
       | Pat_Layered of 'var
       | Pat_Typed
       | Pat_App
       | Pat_FlatApp
       | Pat_Var of 'var
       | Pat_SCon of SCon.t
       | Pat_Wild
       | Pat_Tuple
       | Pat_List
       | Pat_Record
       | Pat_FlexibleRecord
       | Pat_Par (* [Pat] *)

       (* Tys *)
       | Tys (* Ty list *)
       (* Ty *)
       | Ty_Tuple
       | Ty_Record (* Label list *)
       | Ty_Var of 'ident
       | Ty_Con of 'ident (* Ty list *)
       | Ty_Par (* [Ty] *)
       | Ty_Arrow (* [Ty, Ty] *)

       | Unparsed


type ('ident, 'var, 'wrap) ast = (('ident, 'var) node, 'wrap) Wrap.t Tree.t

(* val node = Wrap.unwrap o Tree.this *)

(* fun find' reject pred t = *)
(*     let *)
(*       val n = node t *)
(*     in *)
(*       if pred n then *)
(*         [t] *)
(*       else *)
(*         if reject n then *)
(*           nil *)
(*         else *)
(*           List.concat (map (find' reject pred) (Tree.children t)) *)
(*     end *)

(* val find = find' (fn _ => false) *)

(* fun uimp s _ = raise Fail ("Unimplemented: " ^ s) *)
(* val isStrdec : tree -> bool = uimp "isStrdec" *)
(* val isSigdec : tree -> bool = uimp "isSigdec" *)
(* val isFundec : tree -> bool = uimp "isFundec" *)
(* val isStrbind : tree -> bool = uimp "isStrbind" *)
(* val isSigbind : tree -> bool = uimp "isSigbind" *)
(* val isFunbind : tree -> bool = uimp "isFunbind" *)
(* val isFunarg : tree -> bool = uimp "isFunarg" *)
(* val isStrexp : tree -> bool = uimp "isStrexp" *)
(* val isSigcon : tree -> bool = uimp "isSigcon" *)
(* val isSigexp : tree -> bool = uimp "isSigexp" *)
(* val isWherespec : tree -> bool = uimp "isWherespec" *)
(* val isSpec : tree -> bool = uimp "isSpec" *)
(* val isStrdesc : tree -> bool = uimp "isStrdesc" *)
(* val isTydesc : tree -> bool = uimp "isTydesc" *)
(* val isValdesc : tree -> bool = uimp "isValdesc" *)
(* val isExndesc : tree -> bool = uimp "isExndesc" *)
(* val isDec : tree -> bool = uimp "isDec" *)
(* val isLabel : tree -> bool = uimp "isLabel" *)

(* fun isExp t = *)
(*     case node t of *)
(*       Exp_Handle => true *)
(*     | Exp_Orelse => true *)
(*     | Exp_Andalso => true *)
(*     | Exp_Typed => true *)
(*     | Exp_App => true *)
(*     | Exp_FlatApp => true *)
(*     | Exp_Fn => true *)
(*     | Exp_Case => true *)
(*     | Exp_While => true *)
(*     | Exp_If => true *)
(*     | Exp_Raise => true *)
(*     | Exp_Var _ => true *)
(*     | Exp_SCon _ => true *)
(*     | Exp_Selector _ => true *)
(*     | Exp_Record => true *)
(*     | Exp_Unit => true *)
(*     | Exp_Par => true *)
(*     | Exp_Seq => true *)
(*     | Exp_Tuple => true *)
(*     | Exp_List => true *)
(*     | Exp_Let => true *)
(*     | Exp_LetSeq => true *)
(*     | _ => false *)

(* fun isPat t = *)
(*     case node t of *)
(*       Pat_Layered _ => true *)
(*     | Pat_Typed => true *)
(*     | Pat_App => true *)
(*     | Pat_FlatApp => true *)
(*     | Pat_Var _ => true *)
(*     | Pat_SCon _ => true *)
(*     | Pat_Wild => true *)
(*     | Pat_Tuple => true *)
(*     | Pat_List => true *)
(*     | Pat_Record => true *)
(*     | Pat_FlexibleRecord => true *)
(*     | Pat_Par => true *)
(*     | _ => false *)

(* fun isTy t = *)
(*     case node t of *)
(*       Ty_Tuple => true *)
(*     | Ty_Record => true *)
(*     | Ty_Var _ => true *)
(*     | Ty_Con _ => true *)
(*     | Ty_Par => true *)
(*     | Ty_Arrow => true *)
(*     | _ => false *)

(* fun or (p, p') t = p t orelse p' t *)

fun show' node showid showvar d t =
    let
      open Layout
      infix ^^ ++ \ & \\ &&
      val d = Option.map (1 \> op-) d
      fun next s =
          let
            val s = txt s
          in
            case (Tree.children t, d) of
              (nil, _)    => s
            | (_, SOME 0) => s
            | (ts, _)     => s \ (indent 2 o
                                  enumerate ("", Number, "") NONE o
                                  List.map (show' node showid showvar d)) ts
          end
      fun showSCon scon =
          case scon of
            SCon.String s => "\"" ^ s ^ "\""
          | SCon.Char c => "#\"" ^ c ^ "\""
          | SCon.Int i => i
          | SCon.Real r => r
          | SCon.Word w => w
      (* open Report infix ++ *)
      (* fun next s = *)
      (*     let *)
      (*       val s = text s *)
      (*     in *)
      (*       case Tree.children t of *)
      (*         nil => s *)
      (*       | ts => s ++ (indent o column o map show) ts *)
      (*     end *)
      (* fun show id = *)
      (*     let *)
      (*       val id = Wrap.unwrap id *)
      (*       fun iopt (SOME n) = Int.toString n *)
      (*         | iopt NONE = "" *)
      (*     in *)
      (*       (case Ident.fixity id of *)
      (*          Fixity.InfixL n => "(L" ^ iopt n ^ ") " *)
      (*        | Fixity.InfixR n => "(R" ^ iopt n ^ ") " *)
      (*        | Fixity.Nonfix => "" *)
      (*        | Fixity.Op => "(op)" *)
      (*       ) ^ Ident.toString id *)
      (*     end *)
      (* fun showSCon scon = *)
      (*     case scon of *)
      (*       SCon.String s => "\"" ^ s ^ "\"" *)
      (*     | SCon.Char c => "#\"" ^ c ^ "\"" *)
      (*     | SCon.Int i => i *)
      (*     | SCon.Real r => r *)
      (*     | SCon.Word w => w *)
    in
      case node t of
        Rule_Rules => next "Rule_Rules"
      | Rule_Type_Clauses id => next ("Rule_Type_Clauses: " ^ id)
      | Rule_Type_Expressions id => next ("Rule_Type_Expressions: " ^ id)
      | Rule_Scheme => next "Rule_Scheme>"
      | Rule_Clauses => next "Rule_Clauses"
      | Rule_Clause => next "Rule_Clause"
      | Rule_Cstrns => next "Rule_Cstrns"
      | Rule_Cstrn_Rel id => next ("Rule_Cstrn_Rel: " ^ id)
      | Rule_Trans id => next ("Rule_Trans: " ^ id)
      | Rule_Meta_Pat id => next ("Rule_Meta_Pat: " ^ id)
      | Rule_Self => next "Rule_Self"
      (* SML *)
      | Topdecs => next "Topdecs"
      | Strdecs => next "Strdecs"
      | Strdec_Str => next "Strdec_Str"
      | Strdec_Local => next "Strdec_Local"
      | Sigdec_Sig => next "Sigdec_Sig"
      | Fundec_Fun => next "Fundec_Fun"
      | Strbind id => next ("Strbind " ^ showid id)
      | Sigbind id => next ("Sigbind " ^ showid id)
      | Funbind id => next ("Funbind " ^ showid id)
      | Funarg_Structure id => next ("Funarg_Structure " ^ showid id)
      | Funarg_Spec => next "Funarg_Spec"
      | Strexp_Struct => next "Strexp_Struct"
      | Strexp_Let => next "Strexp_Let"
      | Strexp_Con => next "Strexp_Con"
      | Strexp_Fun id => next ("Strexp_Fun " ^ showid id)
      | Strexp_Var id => next ("Strexp_Var " ^ showid id)
      | Sigcon con => next ("Sigcon " ^
                            (case con of
                               Sigcon.Opaque => "Opaque"
                             | Sigcon.Transparent => "Transparent"
                             | Sigcon.None => "None")
                           )
      | Sigexp_Where => next "Sigexp_Where"
      | Sigexp_Spec => next "Sigexp_Spec"
      | Sigexp_Var id => next ("Sigexp_Var " ^ showid id)
      | Wherespecs => next "Wherespecs"
      | Wherespec (ids, id) =>
        next ("Wherespec " ^ Show.list showid ids ^ " " ^ showid id)
      | Spec_Val => next "Spec_Val"
      | Spec_Type => next "Spec_Type"
      | Spec_Typedef => next "Spec_Typedef"
      | Spec_EqType => next "Spec_EqType"
      | Spec_Datatype => next "Spec_Datatype"
      | Spec_Replication _ => next "Spec_Replication _"
      | Spec_Exception => next "Spec_Exception"
      | Spec_Structure => next "Spec_Structure"
      | Spec_Include => next "Spec_Include"
      | Spec_IncludeSigids _ => next "Spec_IncludeSigids _"
      | Spec_Sharing ids => next ("Spec_Sharing " ^ Show.list showid ids)
      | Spec_SharingStructure ids =>
        next ("Spec_SharingStructure " ^ Show.list showid ids)
      | Strdesc id => next ("Strdesc " ^ showid id)
      | Tydesc (ids, id) =>
        next ("Tydesc " ^ Show.list showid ids ^ " " ^ showid id)
      | Valdesc id => next ("Valdesc " ^ showid id)
      | Exndesc id => next ("Exndesc " ^ showid id)
      | Datatypes => next "Datatypes"
      | Datatype (ids, id) =>
        next ("Datatype " ^ Show.list showid ids ^ " " ^ showid id)
      | Constructor id => next ("Constructor " ^ showvar id)
      | Replication (id, id') =>
        next ("Replication " ^ showvar id ^ " " ^ showvar id')
      | MaybeTy => next "MaybeTy"
      | Decs => next "Decs"
      | Dec_Local => next "Dec_Local"
      | Dec_Val ids => next ("Dec_Val " ^ Show.list showid ids)
      | Dec_Fun ids => next ("Dec_Fun " ^ Show.list showid ids)
      | Dec_Type => next "Dec_Type"
      | Dec_Datatype => next "Dec_Datatype"
      | Dec_Replication (id, id') =>
        next ("Dec_Replication " ^ showid id ^ " " ^ showid id')
      | Dec_Abstype => next "Dec_Abstype"
      | Dec_Exception => next "Dec_Exception"
      | Dec_Open ids => next ("Dec_Open " ^ Show.list showid ids)
      | Dec_Fix (f, ids) =>
        next ("Dec_Fix " ^
              (case f of
                 Fixity.InfixL n => "InfixL " ^ Show.option Show.int n ^ " "
               | Fixity.InfixR n => "InfixR " ^ Show.option Show.int n ^ " "
               | Fixity.Nonfix => "Nonfix "
               | Fixity.Op => "Op ") ^
              Show.list showid ids)
      | Dec_Overload (_, id, _) => next ("Dec_Overload " ^ showvar id)
      | Valbind_Plain => next "Valbind_Plain"
      | Valbind_Rec => next "Valbind_Rec"
      | Match => next "Match"
      | Clause id => next ("Clause " ^ showvar id)
      | FlatClause => next "FlatClause"
      | Rule => next "Rule"
      | Datbinds => next "Datbinds"
      | Withtypes => next "Withtypes"
      | Tybind (ids, id) =>
        next ("Tybind " ^ Show.list showid ids ^ " " ^ showid id)
      | Exps => next "Exps"
      | Exp_Handle => next "Exp_Handle"
      | Exp_Orelse => next "Exp_Orelse"
      | Exp_Andalso => next "Exp_Andalso"
      | Exp_Typed => next "Exp_Typed"
      | Exp_App => next "Exp_App"
      | Exp_FlatApp => next "Exp_FlatApp"
      | Exp_Fn => next "Exp_Fn"
      | Exp_Case => next "Exp_Case"
      | Exp_While => next "Exp_While"
      | Exp_If => next "Exp_If"
      | Exp_Raise => next "Exp_Raise"
      | Exp_Var id => next ("Exp_Var " ^ showvar id)
      | Exp_SCon scon => next ("Exp_SCon " ^ showSCon scon)
      | Exp_Selector id => next ("Exp_Selector " ^ showid id)
      | Exp_Record => next "Exp_Record"
      | Exp_Unit => next "Exp_Unit"
      | Exp_Par => next "Exp_Par"
      | Exp_Seq => next "Exp_Seq"
      | Exp_Tuple => next "Exp_Tuple"
      | Exp_List => next "Exp_List"
      | Exp_Let => next "Exp_Let"
      | Exp_LetSeq => next "Exp_LetSeq"
      | Label_Plain id => next ("Label_Plain " ^ showid id)
      | Label_Short id => next ("Label_Short " ^ showvar id)
      | MaybePat => next "MaybePat"
      | Pats => next "Pats"
      | Pat_Layered var => next ("Pat_Layered " ^ showvar var)
      | Pat_Typed => next "Pat_Typed"
      | Pat_App => next "Pat_App"
      | Pat_FlatApp => next "Pat_FlatApp"
      | Pat_Var id => next ("Pat_Var " ^ showvar id)
      | Pat_SCon scon => next ("Pat_SCon " ^ showSCon scon)
      | Pat_Wild => next "Pat_Wild"
      | Pat_Tuple => next "Pat_Tuple"
      | Pat_List => next "Pat_List"
      | Pat_Record => next "Pat_Record"
      | Pat_FlexibleRecord => next "Pat_FlexibleRecord"
      | Pat_Par => next "Pat_Par"
      | Tys => next "Tys"
      | Ty_Tuple => next "Ty_Tuple"
      | Ty_Record => next "Ty_Record"
      | Ty_Var id => next ("Ty_Var " ^ showid id)
      | Ty_Con id => next ("Ty_Con " ^ showid id)
      | Ty_Par => next "Ty_Par"
      | Ty_Arrow => next "Ty_Arrow"
      | Unparsed => next "Unparsed"
    end

fun show t = show' (Wrap.unwrap o Tree.this) t
fun showUnwrapped t = show' Tree.this t


fun transform var t =
    Tree.join
      (Wrap.modify
         (fn Rule_Rules                => Rule_Rules
           | Rule_Type_Clauses x       => Rule_Type_Clauses x
           | Rule_Type_Expressions x   => Rule_Type_Expressions x
           | Rule_Scheme               => Rule_Scheme
           | Rule_Clauses              => Rule_Clauses
           | Rule_Clause               => Rule_Clause
           | Rule_Cstrns               => Rule_Cstrns
           | Rule_Cstrn_Rel x          => Rule_Cstrn_Rel x
           | Rule_Trans x              => Rule_Trans x
           | Rule_Meta_Pat x           => Rule_Meta_Pat x
           | Rule_Self                 => Rule_Self
           | Topdecs                   => Topdecs
           | Strdecs                   => Strdecs
           | Strdec_Str                => Strdec_Str
           | Strdec_Local              => Strdec_Local
           | Sigdec_Sig                => Sigdec_Sig
           | Fundec_Fun                => Fundec_Fun
           | Strbind x                 => Strbind x
           | Sigbind x                 => Sigbind x
           | Funbind x                 => Funbind x
           | Funarg_Structure x        => Funarg_Structure x
           | Funarg_Spec               => Funarg_Spec
           | Strexp_Struct             => Strexp_Struct
           | Strexp_Let                => Strexp_Let
           | Strexp_Con                => Strexp_Con
           | Strexp_Fun x              => Strexp_Fun x
           | Strexp_Var x              => Strexp_Var x
           | Sigcon x                  => Sigcon x
           | Sigexp_Where              => Sigexp_Where
           | Sigexp_Spec               => Sigexp_Spec
           | Sigexp_Var x              => Sigexp_Var x
           | Wherespecs                => Wherespecs
           | Wherespec x               => Wherespec x
           | Spec_Val                  => Spec_Val
           | Spec_Type                 => Spec_Type
           | Spec_Typedef              => Spec_Typedef
           | Spec_EqType               => Spec_EqType
           | Spec_Datatype             => Spec_Datatype
           | Spec_Replication x        => Spec_Replication x
           | Spec_Exception            => Spec_Exception
           | Spec_Structure            => Spec_Structure
           | Spec_Include              => Spec_Include
           | Spec_IncludeSigids x      => Spec_IncludeSigids x
           | Spec_Sharing x            => Spec_Sharing x
           | Spec_SharingStructure x   => Spec_SharingStructure x
           | Strdesc x                 => Strdesc x
           | Tydesc x                  => Tydesc x
           | Valdesc x                 => Valdesc x
           | Exndesc x                 => Exndesc x
           | Datatypes                 => Datatypes
           | Datatype x                => Datatype x
           | Constructor id            => Constructor $ Wrap.modify var id
           | Replication (id1, id2)    => Replication (Wrap.modify var id1,
                                                    Wrap.modify var id2)
           | MaybeTy                   => MaybeTy
           | Decs                      => Decs
           | Dec_Local                 => Dec_Local
           | Dec_Val x                 => Dec_Val x
           | Dec_Fun x                 => Dec_Fun x
           | Dec_Type                  => Dec_Type
           | Dec_Datatype              => Dec_Datatype
           | Dec_Replication x         => Dec_Replication x
           | Dec_Abstype               => Dec_Abstype
           | Dec_Exception             => Dec_Exception
           | Dec_Open x                => Dec_Open x
           | Dec_Fix x                 => Dec_Fix x
           | Dec_Overload (p, id, ids) =>
             Dec_Overload (p, Wrap.modify var id, ids)
           | Valbind_Plain             => Valbind_Plain
           | Valbind_Rec               => Valbind_Rec
           | Match                     => Match
           | Clause id                 => Clause $ Wrap.modify var id
           | FlatClause                => FlatClause
           | Rule                      => Rule
           | Datbinds                  => Datbinds
           | Withtypes                 => Withtypes
           | Tybind x                  => Tybind x
           | Exps                      => Exps
           | Exp_Handle                => Exp_Handle
           | Exp_Orelse                => Exp_Orelse
           | Exp_Andalso               => Exp_Andalso
           | Exp_Typed                 => Exp_Typed
           | Exp_App                   => Exp_App
           | Exp_FlatApp               => Exp_FlatApp
           | Exp_Fn                    => Exp_Fn
           | Exp_Case                  => Exp_Case
           | Exp_While                 => Exp_While
           | Exp_If                    => Exp_If
           | Exp_Raise                 => Exp_Raise
           | Exp_Var id                => Exp_Var $ Wrap.modify var id
           | Exp_SCon x                => Exp_SCon x
           | Exp_Selector x            => Exp_Selector x
           | Exp_Record                => Exp_Record
           | Exp_Unit                  => Exp_Unit
           | Exp_Par                   => Exp_Par
           | Exp_Seq                   => Exp_Seq
           | Exp_Tuple                 => Exp_Tuple
           | Exp_List                  => Exp_List
           | Exp_Let                   => Exp_Let
           | Exp_LetSeq                => Exp_LetSeq
           | Label_Plain x             => Label_Plain x
           | Label_Short id            => Label_Short $ Wrap.modify var id
           | MaybePat                  => MaybePat
           | Pats                      => Pats
           | Pat_Layered id            => Pat_Layered $ Wrap.modify var id
           | Pat_Typed                 => Pat_Typed
           | Pat_App                   => Pat_App
           | Pat_FlatApp               => Pat_FlatApp
           | Pat_Var id                => Pat_Var $ Wrap.modify var id
           | Pat_SCon x                => Pat_SCon x
           | Pat_Wild                  => Pat_Wild
           | Pat_Tuple                 => Pat_Tuple
           | Pat_List                  => Pat_List
           | Pat_Record                => Pat_Record
           | Pat_FlexibleRecord        => Pat_FlexibleRecord
           | Pat_Par                   => Pat_Par
           | Tys                       => Tys
           | Ty_Tuple                  => Ty_Tuple
           | Ty_Record                 => Ty_Record
           | Ty_Var x                  => Ty_Var x
           | Ty_Con x                  => Ty_Con x
           | Ty_Par                    => Ty_Par
           | Ty_Arrow                  => Ty_Arrow
           | Unparsed                  => Unparsed
         )
         (Tree.this t)
      )
      (List.map (transform var) $ Tree.children t)
end

