structure Variable :> Variable =
struct
type t = Ident.t * ValEnv.vid option

fun ofIdent id = (id, NONE)
fun ident (id, _) = id
fun load (_, SOME vid) = vid
  | load _ = raise Empty
fun store (id, _) vid = (id, SOME vid)

fun show (id, _) = Ident.show id

fun toString (id, SOME vid) = Ident.toString id ^ "(" ^ ValEnv.vidToString vid ^ ")"
  | toString (id, NONE) = Ident.toString id
end
