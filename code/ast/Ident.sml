structure Ident :> Ident =
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


fun toString (s, _) = toString' s

fun show (s, f) =
    Layout.txt (
    if f = Fixity.Op then
      "op " ^ toString' s
    else
      toString' s
    )

fun isQual (s, _) = length s > 1
fun isUnqual id = not (isQual id)
fun isInfix (_, f) =
    case f of
      Fixity.InfixL _ => true
    | Fixity.InfixR _ => true
    | _ => false
val isNonfix = not o isInfix
fun fixity (_, f) = f
fun explode (s, _) =
    case rev s of
      id :: ids => (rev ids, id)
    | _ => raise Domain

fun setFixity (s, _) f = (s, f)
fun opify i = setFixity i Fixity.Op
end
