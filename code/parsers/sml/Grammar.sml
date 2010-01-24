
structure Sigcon =
struct
datatype t = Opaque | Transparent | None
end

structure Fixity =
struct
datatype t = Nonfix | InfixL of int option | InfixR of int option | Op
end

structure SCon =
struct
datatype t = String of string
           | Char of string
           | Int of string
           | Real of string
           | Word of string
end

structure Ident =
struct
type t = string list * Fixity.t

structure Symbols =
struct
val equal = (["="], Fixity.Nonfix)
val asterisk = (["*"], Fixity.Nonfix)
end

fun fromString f s = (String.tokens (fn c => c = #".") s, f)
fun toString' [s] = s
  | toString' (s :: ss) = s ^ "." ^ toString' ss
  | toString' _ = Crash.impossible "Ident.toString"


fun toString (s, f) =
    if f = Fixity.Op then
      "op " ^ toString' s
    else
      toString' s

fun isQual (s, _) = length s > 1
fun fixity (_, f) = f
fun ident (s, _) = toString' s
fun explode (s, _) = s

fun setFixity (s, _) f = (s, f)
fun opify i = setFixity i Fixity.Op
end

structure Wrap =
struct
type 'a t = {node: 'a, left: int, right: int}

fun wrap n l r = {node = n, left = l, right = r}
fun unwrap ({node, ...} : 'a t) = node
fun left ({left, ...} : 'a t) = left
fun right ({right, ...} : 'a t) = right

fun modify f {node, left, right} = {node = f node, left = left, right = right}
end

structure SMLGrammar =
struct
type ident = Ident.t Wrap.t

(*
 Topdec = Strdec U Sigdec U Fundec U Dec U Exp
 Strdec' = Strdec U Dec U Exp
 *)

datatype t = Topdecs (* Topdec list *)

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
           | Strbind of ident (* [Strexp, Sigcon] *)

           (* Sigbind *)
           | Sigbind of ident (* [Sigexp] *)

           (* Funbind *)
           | Funbind of ident (* [Funarg, Strexp, Sigcon] *)

           (* Funarg *)
           | Funarg_Structure of ident (* [Sigexp] *)
           | Funarg_Spec (* Spec list *)

           (* Strexp *)
           | Strexp_Struct (* Strdec' list *)
           | Strexp_Let (* [Strdecs, Strexp] *)
           | Strexp_Con (* [Strexp, Sigcon] *)
           | Strexp_Fun of ident (* [Strexp | Strdecs] *)
           | Strexp_Var of ident

           (* Sigcon *)
           | Sigcon of Sigcon.t (* [Sigexp] | [] *)

           (* Sigexp *)
           | Sigexp_Where (* [Sigexp, Wherespecs] *)
           | Sigexp_Spec (* Spec list *)
           | Sigexp_Var of ident

           (* Wherespecs *)
           | Wherespecs (* Wherespec list *)

           (* Wherespec *)
           | Wherespec of ident list * ident (* [Ty] *)

           (* Spec *)
           | Spec_Val (* Valdesc list *)
           | Spec_Type (* Tydesc list *)
           | Spec_Typedef (* Tybind list *)
           | Spec_EqType (* Tydesc list *)
           | Spec_Datatype (* Datdesc list *)
           (* Replication *)
           | Spec_Exception (* Exndesc list *)
           | Spec_Structure (* Strdesc list *)
           | Spec_Include (* [Sigexp] *)
           | Spec_IncludeSigids of ident list
           | Spec_Sharing of ident list
           | Spec_SharingStructure of ident list

           (* Strdesc *)
           | Strdesc of ident (* [Sigexp] *)

           (* Tydesc *)
           | Tydesc of ident list * ident

           (* Valdesc *)
           | Valdesc of ident (* [Ty] *)

           (* Exndesc *)
           | Exndesc of ident (* [MaybeTy] *)

           (* Datatypes *)
           | Datatypes (* Datatype list *)
           (* Datatype *)
           | Datatype of ident list * ident (* Constructor list] *)
           (* Constructor *)
           | Constructor of ident (* [Con, MaybeTy] *)
           (* Replication *)
           | Replication of ident * ident

           (* MaybeTy *)
           | MaybeTy (* [Ty] | [] *)

           (* Decs *)
           | Decs (* Dec list *)
           (* Dec *)
           | Dec_Local (* [Decs, Decs] *)
           | Dec_Val of ident list (* Valbind list *)
           | Dec_Fun of ident list (* Match list (Clause) *)
           | Dec_Type (* Tybind list *)
           | Dec_Datatype (* [Datatypes, Withtypes] *)
           (* Replication *)
           | Dec_Abstype (* [Datbinds, Withtypes, Decs] *)
           | Dec_Exception (* (Constructor | Replication) list *)
           | Dec_Open of ident list
           | Dec_Fix of Fixity.t * ident list
           | Dec_Overload of int option * ident * ident list (* [Ty] *)

           (* Valbinds *)
           | Valbinds (* Valbind list *)

           (* Valbind *)
           | Valbind_Plain (* [Pat, Exp] *)
           | Valbind_Rec (* [Pat, Match] *)

           (* Matches *)
           | Matches (* Match list *)
           (* Match *)
           | Match (* (Clause | Rule) list *)
           (* Clause *)
           | Clause (* [Pats, MaybeTy, Exp] *)
           | Rule (* [Pat, Exp] *)

           (* Datbinds *)
           | Datbinds (* Datatype list *)

           (* Withtypes *)
           | Withtypes (* Tybind list *)
           (* Tybind *)
           | Tybind of ident list * ident (* [Ty] *)

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
           | Exp_Var of ident
           | Exp_SCon of SCon.t
           | Exp_Selector of ident
           | Exp_Record (* Label list *)
           | Exp_Unit
           | Exp_Par (* [Exp] *)
           | Exp_Seq (* Exp list *)
           | Exp_Tuple (* Exp list *)
           | Exp_List (* Exp list *)
           | Exp_Let (* [Decs, Exp] *)
           | Exp_LetSeq (* [Decs, Exps] *)

           (* Label *)
           | Label of ident (* [Exp | Pat | Ty] *)
           | Label_Short of ident (* [MaybeTy, MaybePat] *)

           (* MaybePat *)
           | MaybePat (* [Pat] | [] *)
           (* Pats *)
           | Pats (* Pat list *)
           (* Pat *)
           | Pat_Layered of ident
           | Pat_Typed
           | Pat_App
           | Pat_FlatApp
           | Pat_Var of ident
           | Pat_SCon of SCon.t
           | Pat_Wild
           | Pat_Tuple
           | Pat_List
           | Pat_Record
           | Pat_FlexibleRecord

           (* Tys *)
           | Tys (* Ty list *)
           (* Ty *)
           | Ty_Tuple
           | Ty_Record (* Label list *)
           | Ty_Var of ident
           | Ty_Con of ident (* Ty list *)
           | Ty_Par (* [Ty] *)
           | Ty_Arrow (* [Ty, Ty] *)


type wrapped = t Wrap.t

type tree = wrapped Tree.t
end
