structure IntEnv =
struct
datatype t =
         I of ValEnv.t Dictionary.t *
              ((Grammar.ident, Grammar.variable, int) Grammar.ast *
               Ident.t option *
               ValEnv.t *
               ValEnv.t option *
               t *
               ValEnv.t) Dictionary.t

val empty = I (Dictionary.empty, Dictionary.empty)

fun fail s = raise Fail s

fun idToString id =
    if Ident.isUnqual id then
      Ident.toString id
    else
      fail ("Qualified identifier in functor or signature: " ^ Ident.toString id)

fun bindSig (I (sigs, funs)) (id, env) =
    I (Dictionary.update sigs (idToString id, env), funs)

fun bindFun (I (sigs, funs)) (id, x) =
    I (sigs, Dictionary.update funs (idToString id, x))
    (* before *)
    (* println ("Functor: " ^ idToString id) *)

fun findFun (I (sigs, funs)) id =
    case Dictionary.lookup funs $ idToString id of
      SOME x => x
    | NONE   => fail ("Undefined functor: " ^ idToString id)

fun findSig (I (sigs, funs)) id =
    case Dictionary.lookup sigs $ idToString id of
      SOME x => x
    | NONE   => fail ("Undefined signature: " ^ idToString id)

infix ++
fun (I (si1, ti1)) ++ (I (si2, ti2)) =
    I (Dictionary.plus si1 si2, Dictionary.plus ti1 ti2)

fun sigRep (I (sigs, _)) (id1, id2) =
    case Dictionary.lookup sigs id2 of
      SOME s => I (Dictionary.singleton (id1, s), Dictionary.empty)
    | NONE   => fail ("Undefined signature: " ^ id2)

fun funRep (I (_, funs)) (id1, id2) =
    case Dictionary.lookup funs id2 of
      SOME f => I (Dictionary.empty, Dictionary.singleton (id1, f))
    | NONE   => fail ("Undefined signature: " ^ id2)
end
