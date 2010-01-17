structure Ident =
type t = string
fun fromString s = s
end

structure LongIdent =
type t = string list
val fromString = String.tokens (fn c => c = #".")
end

structure Sigcon =
struct
datatype t = Opaque | Transparent | None
end

(* Categories *)
(*
 Strdec
 Sigdec
 Fundec
 Dec
 Exp
 *)

(* Compound categories *)
(*
 Topdec = Strdec U Sigdec U Fundec U Dec U Exp
 Strdec' = Strdec U Dec U Exp
 *)

datatype t = Topdecs (* Topdec list *)

           (* Strdec *)
           | Strdec_Str   (* Strbind list *)
           | Strdec_Local (* [Strdecs, Strdecs] *)

           (* Strdecs *)
           | Strdecs (* Strdec' list *)

           (* Sigdec *)
           | Sigdec_Sig (* Sigbind list *)

           (* Fundec *)
           | Fundec_Fun (* Funbind list *)

           (* Strbind *)
           | Strbind (* [Ident, Strexp, Sigcon] *)

           (* Sigbind *)
           | Sigbind (* [Ident, Sigexp] *)

           (* Funbind *)
           | Funbind (* [Ident, Funarg] *)

           (* Funarg *)
           | Funarg_Structure (* [Ident, Sigexp] *)
           | Funarg_Spec (* Spec list *)

           (* Strexp *)
           | Strexp_Struct (* Strdec' list *)
           | Strexp_Let (* [Strdecs, Strexp] *)
           | Strexp_Con (* [Strexp, Sigcon] *)
           | Strexp_Fun (* [LongIdent, Strexp | Strdecs] *)
           (* LongIdent *)

           (* Sigcon *)
           | Sigcon of Sigcon.t (* [Sigexp] | [] *)

           (* Sigexp *)
           | Sigexp_Where (* [Sigexp, Wherespecs] *)
           | Sigexp_Spec (* Spec list *)
           (* Ident *)

           (* Wherespecs *)
           | Wherespecs (* Wherespec list *)

           (* Wherespec *)
           | Wherespec (* [Tyvars, LongIdent, Ty] *)

           (* Spec *)
           | Spec_Val (* Valdesc list *)
           | Spec_Type (* Tydesc list *)
           | Spec_Typedef (* Tybind list *)
           | Spec_EqType (* Tydesc list *)
           | Spec_Datatype (* Datdesc list *)
           | Spec_Replication (* [Ident, LongIdent] *)
           | Spec_Exception (* Exdesc list *)
           | Spec_Structure (* Strdesc list *)
           | Spec_Include (* [sigexp] *)
           | Spec_IncludeSigids (* Ident list *)
           | Spec_Sharing (* LongIdent list *)
           | Spec_SharingStructure (* LongIdent list *)

           (* Strdesc *)
           | Strdesc (* [Ident, Sigexp] *)

           (* Tydesc *)
           | Tydesc (* [Tyvars, Ident] *)

           (* Valdesc *)
           | Valdesc (* [Ident, Ty] *)

           (* Exndesc *)
           | Exndesc (* [Ident, MaybeTy] *)

           (* MaybeTy *)
           | MaybeTy (* [Ty] | [] *)

           (* Tyvars *)
           | Tyvars (* Ident list *)

           (* Ident *)
           | Ident of Ident.t

           (* LongIdent *)
           | LongIdent of LongIdent.t


type node = t Wrap.t
