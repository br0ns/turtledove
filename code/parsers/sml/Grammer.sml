datatype t = Toplevel (* (Topdec_ | Strdec_ | Dec_ | Exp_ ) list *)

           | Topdec_Str (* Strbind list *)
           | Topdec_Sig (* Sigbind list *)
           | Topdec_Fun (* Funbind list *)

           | Strbind (* [Strid, Strid, Sigcon_] *)
           | Sigbind (* [

           | Strdecs (* (Strdec_ | Dec_) list *)

           | Strdec_Local (* [Strdecs, Strdecs] *)



type node = t Wrap.t
