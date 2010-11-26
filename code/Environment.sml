structure Environment :> Environment =
struct
open IntInf Dictionary
type 'a dict = 'a Dictionary.t
val n = ref 0

type ident = string
type tycon = string
type idref = int

datatype status =
         Val
       | Con of tycon
       | Exn

type vid = (idref, SCon.t) either * status
type dat = StringSet.t
datatype t = E of vid dict * dat dict * t dict

val empty = (empty, empty, empty)

fun bind (E (ve, te, se)) id stat =
    E (insert ve (id, (Left $ inc n, obj)), te, se)
fun bindSCon (E (ve, te, se)) id scon =
    E (insert ve (id, (Right scon, Val)), te, se)
fun bindDat (E (ve, te, se)) id cons =
    E (ve, insert te (id, cons), se)
fun bindStruct (E (ve, te, se)) id s =
    E (ve, te, insert se (id, s))

val new = bind empty
val newDat = bindDat empty
val newSCon = bindSCon empty
fun newVal id = new id Val
fun newCon id tycon = new id $ Con tycon
fun newExn id = new id Exn

infix ++
fun (E (ve1, te1, se1)) ++ (E (ve2, te2, se2)) =
    E (plus ve1 ve2, plus te1 te2, plus se1 se2)

fun lookup (E (ve, te, se)) id = Dictionary.lookup ve id
fun lookupDat (E (ve, te, se)) id = Dictionary.lookup te id
fun lookupStruct (E (ve, te, se)) id = Dictionary.lookup se id

fun equiv (v1, _) (v2, _) =
    case (v1, v2) of
      (Left n1, Left n2)   => !n1 = !n2
    | (Right s1, Right s2) => s1 = s2
    | _ => false

fun findStruct e ids =
    foldl
      (fn (id, e) =
          case lookupStruct e of
            SOME e => e
          | NONE   => raise Domain
      )
      e ids

fun find' lookup e ident =
    let
      val (ids, id) = Ident.explode ident
      val e = findStruct e ids
    in
      case lookup e id of
        SOME x => x
      | NONE   => raise Domain
    end

val find = find' lookup
val findDat = find' lookupDat

(* fun EtoI e =  *)
(* fun ItoE *)
end
