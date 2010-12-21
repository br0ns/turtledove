(* TODO: Give meaningful errors *)

structure ValEnv =
struct
fun fail s = raise Fail s


open (* IntInf *) Dictionary
type 'a dict = 'a Dictionary.t
val n = ref 0

type ident = string
type tycon = string
type idref = int

datatype status =
         Val
       | Con of vid dict
       | Exn
withtype vid = idref * (idref, SCon.t) either * status

(* idents * datatypes * structures *)
datatype t = E of vid dict * vid dict dict * t dict

fun show (E (ve, te, se)) =
    let
      open Layout infix ++ \ ^^
      fun sv (id, (_, _, stat)) =
          case stat of
            Exn   => SOME (txt "exception" ++ txt id)
          | Val   => SOME (txt "val" ++ txt id)
          | Con _ => NONE

      fun st (id, ve) =
          SOME (
          txt "datatype" ++ txt id ++ align
              (txt "=" ++ hcat (punctuate (ln ^^ txt "| ") $ List.map txt $ domain ve))
          )

      fun ss (id, e) =
          SOME (
          txt "structure" ++ txt id ++ txt "= struct" \
              indent 2 (show e) \
              txt "end"
          )
      fun list s e = vsep $ List.mapPartial s $ toList e
    in
      list sv ve \ list st te \ list ss se
    end

fun idToString id =
    if Ident.isUnqual id then
      Ident.toString id
    else
      fail ("Qualified identifier in functor or signature: " ^ Ident.toString id)

fun rebind (id, v, stat) =
    E (singleton (idToString id, (inc n, v, stat)), empty, empty)

fun new (id, stat) =
    let
      val idref = inc n
    in
      E (singleton (idToString id, (idref, Left idref, stat)), empty, empty)
    end

fun newSCon (id, scon) =
    let
      val idref = inc n
    in
      E (singleton (idToString id, (idref, Right scon, Val)), empty, empty)
    end

fun newStr (id, str) =
    E (empty, empty, singleton (idToString id, str))

fun newVal id = new (id, Val)
fun newExn id = new (id, Exn)

fun newDat' (id, ve) =
    E (map (fn (id, v, _) => (id, v, Con ve)) ve,
       singleton (idToString id, ve),
       empty)

fun newDat (id, E (ve, _, _)) =
    newDat' (id, ve)

infix ++
fun (E (ve1, te1, se1)) ++ (E (ve2, te2, se2)) =
    E (plus ve1 ve2, plus te1 te2, plus se1 se2)

fun lookupVal (E (ve, te, se)) id = lookup ve id
fun lookupDat (E (ve, te, se)) id = lookup te id
fun lookupStr (E (ve, te, se)) id = lookup se id

fun same (id1, _, _) (id2, _, _) = id1 = id2
fun equiv (_, v1, _) (_, v2, _) = v1 = v2

fun findEquiv (E (ve, _, se)) vid =
    let
      val ve' = filter (equiv vid) ve
      fun loop nil = nil
        | loop ((sid, e) :: se) =
          (List.map (fn id => sid ^ "." ^ id) $ findEquiv e vid)
          :: loop se
    in
      List.concat (domain ve' :: loop (toList se))
    end

fun findStr' e ids =
    List.foldl
      (fn (id, e) =>
          case lookupStr e id of
            SOME e => e
          | NONE   => raise Domain
      )
      e ids

fun find' lookup e ident =
    let
      val (ids, id) = Ident.explode ident
      val e = findStr' e ids
    in
      case lookup e id of
        SOME x => x
      | NONE   => raise Domain
    end

fun find lookup env id = find' lookup env id
    (* handle Domain => fail ("Undefined identifier:" ^ Ident.toString id) *)
val findStr = find lookupStr
val findVal = find lookupVal
val findDat = find lookupDat

fun isConOrExn env id =
    (case findVal env id of
       (_, _, Val) => false
     | _           => true)
    handle Domain => false

fun exnRep env (e1, e2) =
    let
      val (_, v, stat) = findVal env e2
    in
      case stat of
        Exn => rebind (e1, v, Exn)
      | _   => fail ("Not an exception constructor: " ^ Ident.toString e2)
    end

fun datRep env (d1, d2) =
    newDat' (d1, findDat env d2)
    handle Domain => raise Fail (Ident.toString d1 ^ " = " ^ Ident.toString d2)
                           before Layout.println NONE $ show env

fun valRep env (id1, id2) =
    let
      val (_, v, stat) = findVal env id2
    in
      rebind (id1, v, stat)
    end

fun strRep env (id1, id2) =
    let
      val str = findStr env $ Ident.fromString Fixity.Nonfix id2
    in
      newStr (Ident.fromString Fixity.Nonfix id1, str)
    end

(* TODO: Assert fulfilled constrain *)
fun constrain (e1 as E (ve, te, se)) (e2 as E (vi, ti, si)) =
    let
      (* val _ = println "constrain" *)
      (* val _ = Layout.println NONE $ show e1 *)
      (* val _ = Layout.println NONE $ show e1 *)
      fun con e i =
          Dictionary.mapi
            (fn (id, _) =>
                case Dictionary.lookup e id of
                  SOME x => x
                | NONE   => fail "Constraint not fulfilled"
            )
            i

      val ve = con ve vi
      val te = con te ti
      val se =
          Dictionary.mapi
            (fn (id, i) =>
                case Dictionary.lookup se id of
                  SOME e => constrain e i
                | NONE   => fail "Constraint not fulfilled"
            )
            si
    in
      E (ve, te, se)
    end

val empty = E (empty, empty, empty)

local
  fun toId id = Ident.fromString Fixity.Nonfix id
  fun v ids =
      List.foldl
        (fn (id, env) => env ++ newVal (toId id))
        empty
        ids
  fun e ids =
      List.foldl
        (fn (id, env) => env ++ newExn (toId id))
        empty
        ids
  fun d dats =
      List.foldl
        (fn ((id, cons), env) => env ++ newDat (toId id, v cons))
        empty
        dats
  val dats = d [("bool", ["true", "false"]),
                ("list", ["nil", "::"]),
                ("ref", nil)
               ]
                (* ("order", ["LESS", "EQUAL", "GREATER"]), *)
                (* ("option", ["NONE", "SOME"]) *)
               (* ] *)
  val exns = e ["Bind",
                "Match",
                "Overflow"
               ]
in
val prim = dats ++ exns
end
end
