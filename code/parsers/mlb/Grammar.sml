structure MLBGrammar =
struct
type basid = string
type basids = basid list
type file = string

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
                | File of file
                | Open of basids
                | Ann of string list * basdecs
                | Structure of strbinds
                | Signature of sigbinds
                | Functor of fctbinds
                | Prim
withtype basdecs = basdec list
     and basbinds = (basid * basexp) list
end
