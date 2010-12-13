structure ParserUtils =
struct
open Grammar
type comments = SourceData.Comments.t
val dummypos = ~1

val join = Tree.join

val wrap = Wrap.wrap
fun left t = Wrap.left $ Tree.this t
fun right t = Wrap.right $ Tree.this t
val unwrap = Wrap.unwrap

fun leftmost ts = left $ List.hd ts
fun rightmost ts = right $ List.last ts

fun node t = Wrap.unwrap $ Tree.this t
val children = Tree.children

fun fail p s = raise YaccError (p, s)
fun die s = Crash.impossible s

fun opify t = Wrap.modify Ident.opify t

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
        fail p s
    end

fun ensureUnqual i =
    if Ident.isQual (unwrap i) then
      fail (Wrap.left i) "Identifier must be unqualified"
    else
      i

fun mkIdent' l r f s = wrap (Ident.fromString f s) l r

fun mkIdent l r f s = ensureUnqual (mkIdent' l r f s)
fun mkLongIdent l r s = mkIdent' l r Fixity.Nonfix s

fun mkTyvar l r s =
    if String.sub (s, 0) = #"'" then
      mkIdent' l r Fixity.Nonfix s
    else
      fail l "Type variable must start with a pling (')"
end
