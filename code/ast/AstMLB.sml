structure AstMLB =
struct
type basid = string
type basids = basid list
type file = File.t

(* Qualified identifiers are not allowed in MLB files, so by the KISS
   principle we just use strings here. *)
type strid = string
type sigid = string
type fctid = string

(* (lhs, rhs) *)
type strbind = strid * strid
type sigbind = sigid * sigid
type fctbind = fctid * fctid

type strbinds = strbind list
type sigbinds = sigbind list
type fctbinds = fctbind list
                
datatype basexp = Bas of basdecs
                | Let of basdecs * basexp
                | Var of basid

     (* Basis is a list of bindings because of the 'and' keyword as in
        basis foo = bas foo.sml end
          and bar = bas bar.sml end
      *)
     and basdec = Basis of basbinds
                | Local of basdecs * basdecs
                | Include of file * basdecs
                | Source of file
                | Open of basids
                | Ann of string list * basdecs
                | Structure of strbinds
                | Signature of sigbinds
                | Functor of fctbinds
                | Prim
withtype basdecs = basdec list
     and basbinds = (basid * basexp) list

local
  open Report
  infix ++
in
fun showBasexp (Bas basdecs) =
    text "bas" ++
         indent (showBasdecs basdecs) ++
         text "end"
  | showBasexp (Let (basdecs, basexp)) =
    text "let" ++
         indent (showBasdecs basdecs) ++
         text "in" ++
         indent (showBasexp basexp) ++
         text "end"
  | showBasexp (Var basid) =
    text basid
and showBasdec (Basis basbinds) =
    showBasbinds basbinds
  | showBasdec (Local (basdecs, basdecs')) =
    text "local" ++
         indent (showBasdecs basdecs) ++
         text "in" ++
         indent (showBasdecs basdecs') ++
         text "end"
  | showBasdec (Include (file, basdecs)) =
    text ("\"" ^ Path.toString file ^ "\":") ++
    indent (showBasdecs basdecs)
  | showBasdec (Source file) = 
    text ("\"" ^ Path.toString file ^ "\"")
  | showBasdec (Open basids) =
    text (foldl (fn (basid, s) => s ^ " " ^ basid) "open" basids)
  | showBasdec (Ann (anns, ds)) =
    text "ann ... in" ++
         indent (showBasdecs ds) ++
         text "end"
  | showBasdec (Structure strbinds) = showStrbinds strbinds
  | showBasdec (Signature sigbinds) = showSigbinds sigbinds
  | showBasdec (Functor fctbinds) = showFctbinds fctbinds
  | showBasdec Prim = text "_prim"
and showBasbind (basid, basexp) =
    text ("basis " ^ basid ^ "=") ++
         indent (showBasexp basexp)
and showBasdecs [basdec] =
    showBasdec basdec
  | showBasdecs (basdec :: basdecs) =
    showBasdec basdec ++
               showBasdecs basdecs
  | showBasdecs _ = text ""
and showBasbinds [basbind] =
    showBasbind basbind
  | showBasbinds (basbind :: basbinds) =
    let
      fun showBasbind' (basid, basexp) =
          text ("  and " ^ basid ^ " =") ++
               indent (showBasexp basexp)
      fun showBasbinds' [basbind] =
          showBasbind' basbind
        | showBasbinds' (basbind :: basbinds) =
          showBasbind' basbind ++
                       showBasbinds' basbinds
        | showBasbinds' _ = Crash.impossible "MLBGrammar.showBasbinds'"
    in
      showBasbind basbind ++
                  showBasbinds' basbinds
    end
  | showBasbinds _ = Crash.impossible "MLBGrammar.showBasbinds"
and showStrbinds [strbind] = showStrbind strbind
  | showStrbinds (strbind :: binds) =
    showStrbind strbind ++ showBinds binds
  | showStrbinds _ = Crash.impossible "MLBGrammar.showStrbinds"
and showSigbinds [sigbind] = showSigbind sigbind
  | showSigbinds (sigbind :: binds) =
    showSigbind sigbind ++ showBinds binds
  | showSigbinds _ = Crash.impossible "MLBGrammar.showSigbinds"
and showFctbinds [fctbind] = showFctbind fctbind
  | showFctbinds (fctbind :: binds) =
    showFctbind fctbind ++ showBinds binds
  | showFctbinds _ = Crash.impossible "MLBGrammar.showFctbinds"
and showStrbind (id, id') =
    text ("structure " ^ id ^ " = " ^ id')
and showSigbind (id, id') =
    text ("signature " ^ id ^ " = " ^ id')
and showFctbind (id, id') =
    text ("functor " ^ id ^ " = " ^ id')
and showBinds [bind] = showBind bind
  | showBinds (bind :: binds) =
    showBind bind ++ showBinds binds
  | showBinds _ = Crash.impossible "MLBGrammar.showBinds"
and showBind (id, id') =
    text ("and " ^ id ^ " = " ^ id')

end
end
