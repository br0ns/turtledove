structure NormalForm =
struct

fun unwrap t =
    let open Grammar in
      Tree.join
        (case Wrap.unwrap $ Tree.this t of
           Strbind x                 => Strbind $ Wrap.unwrap x
         | Sigbind x                 => Sigbind $ Wrap.unwrap x
         | Funbind x                 => Funbind $ Wrap.unwrap x
         | Funarg_Structure x        => Funarg_Structure $ Wrap.unwrap x
         | Strexp_Fun x              => Strexp_Fun $ Wrap.unwrap x
         | Strexp_Var x              => Strexp_Var $ Wrap.unwrap x
         | Sigexp_Var x              => Sigexp_Var $ Wrap.unwrap x
         | Wherespec (xs, x)         => Wherespec $ (map Wrap.unwrap xs, Wrap.unwrap x)
         | Spec_Replication (x1, x2) => Spec_Replication (Wrap.unwrap x1, Wrap.unwrap x2)
         | Spec_IncludeSigids xs     => Spec_IncludeSigids $ map Wrap.unwrap xs
         | Spec_Sharing xs           => Spec_Sharing $ map Wrap.unwrap xs
         | Spec_SharingStructure xs  => Spec_SharingStructure $ map Wrap.unwrap xs
         | Strdesc x                 => Strdesc $ Wrap.unwrap x
         | Tydesc (xs, x)            => Tydesc (map Wrap.unwrap xs, Wrap.unwrap x)
         | Valdesc x                 => Valdesc $ Wrap.unwrap x
         | Exndesc x                 => Exndesc $ Wrap.unwrap x
         | Datatype (xs, x)          => Datatype (map Wrap.unwrap xs, Wrap.unwrap x)
         | Constructor x             => Constructor $ Wrap.unwrap x
         | Replication (x1, x2)      => Replication (Wrap.unwrap x1, Wrap.unwrap x2)
         | Dec_Val xs                => Dec_Val $ map Wrap.unwrap xs
         | Dec_Fun xs                => Dec_Fun $ map Wrap.unwrap xs
         | Dec_Replication (x1, x2)  => Dec_Replication (Wrap.unwrap x1, Wrap.unwrap x2)
         | Dec_Open xs               => Dec_Open $ map Wrap.unwrap xs
         | Dec_Fix (f, xs)           => Dec_Fix (f, map Wrap.unwrap xs)
         | Dec_Overload (p, x, xs)   => Dec_Overload (p, Wrap.unwrap x, map Wrap.unwrap xs)
         | Clause x                  => Clause $ Wrap.unwrap x
         | Tybind (xs, x)            => Tybind (map Wrap.unwrap xs, Wrap.unwrap x)
         | Exp_Var x                 => Exp_Var $ Wrap.unwrap x
         | Exp_Selector x            => Exp_Selector $ Wrap.unwrap x
         | Label_Plain x             => Label_Plain $ Wrap.unwrap x
         | Label_Short x             => Label_Short $ Wrap.unwrap x
         | Pat_Layered x             => Pat_Layered $ Wrap.unwrap x
         | Pat_Var x                 => Pat_Var $ Wrap.unwrap x
         | Ty_Var x                  => Ty_Var $ Wrap.unwrap x
         | Ty_Con x                  => Ty_Con $ Wrap.unwrap x

         (* The rest *)
         | Rule_Rules                => Rule_Rules
         | Rule_Type_Clauses x       => Rule_Type_Clauses x
         | Rule_Type_Expressions x   => Rule_Type_Expressions x
         | Rule_Scheme               => Rule_Scheme
         | Rule_Clauses              => Rule_Clauses
         | Rule_Clause               => Rule_Clause
         | Rule_Cstrns               => Rule_Cstrns
         | Rule_Cstrn_Rel x          => Rule_Cstrn_Rel x
         | Rule_Trans x              => Rule_Trans x
         | Rule_Meta_Pat x           => Rule_Meta_Pat x
         | Rule_Self                 => Rule_Self
         | Topdecs                   => Topdecs
         | Strdecs                   => Strdecs
         | Strdec_Str                => Strdec_Str
         | Strdec_Local              => Strdec_Local
         | Sigdec_Sig                => Sigdec_Sig
         | Fundec_Fun                => Fundec_Fun
         | Funarg_Spec               => Funarg_Spec
         | Strexp_Struct             => Strexp_Struct
         | Strexp_Let                => Strexp_Let
         | Strexp_Con                => Strexp_Con
         | Sigcon x                  => Sigcon x
         | Sigexp_Where              => Sigexp_Where
         | Sigexp_Spec               => Sigexp_Spec
         | Wherespecs                => Wherespecs
         | Spec_Val                  => Spec_Val
         | Spec_Type                 => Spec_Type
         | Spec_Typedef              => Spec_Typedef
         | Spec_EqType               => Spec_EqType
         | Spec_Datatype             => Spec_Datatype
         | Spec_Exception            => Spec_Exception
         | Spec_Structure            => Spec_Structure
         | Spec_Include              => Spec_Include
         | Datatypes                 => Datatypes
         | MaybeTy                   => MaybeTy
         | Decs                      => Decs
         | Dec_Local                 => Dec_Local
         | Dec_Type                  => Dec_Type
         | Dec_Datatype              => Dec_Datatype
         | Dec_Abstype               => Dec_Abstype
         | Dec_Exception             => Dec_Exception
         | Valbind_Plain             => Valbind_Plain
         | Valbind_Rec               => Valbind_Rec
         | Match                     => Match
         | FlatClause                => FlatClause
         | Rule                      => Rule
         | Datbinds                  => Datbinds
         | Withtypes                 => Withtypes
         | Exps                      => Exps
         | Exp_Handle                => Exp_Handle
         | Exp_Orelse                => Exp_Orelse
         | Exp_Andalso               => Exp_Andalso
         | Exp_Typed                 => Exp_Typed
         | Exp_App                   => Exp_App
         | Exp_FlatApp               => Exp_FlatApp
         | Exp_Fn                    => Exp_Fn
         | Exp_Case                  => Exp_Case
         | Exp_While                 => Exp_While
         | Exp_If                    => Exp_If
         | Exp_Raise                 => Exp_Raise
         | Exp_SCon x                => Exp_SCon x
         | Exp_Record                => Exp_Record
         | Exp_Unit                  => Exp_Unit
         | Exp_Par                   => Exp_Par
         | Exp_Seq                   => Exp_Seq
         | Exp_Tuple                 => Exp_Tuple
         | Exp_List                  => Exp_List
         | Exp_Let                   => Exp_Let
         | Exp_LetSeq                => Exp_LetSeq
         | MaybePat                  => MaybePat
         | Pats                      => Pats
         | Pat_Typed                 => Pat_Typed
         | Pat_App                   => Pat_App
         | Pat_FlatApp               => Pat_FlatApp
         | Pat_SCon x                => Pat_SCon x
         | Pat_Wild                  => Pat_Wild
         | Pat_Tuple                 => Pat_Tuple
         | Pat_List                  => Pat_List
         | Pat_Record                => Pat_Record
         | Pat_FlexibleRecord        => Pat_FlexibleRecord
         | Pat_Par                   => Pat_Par
         | Tys                       => Tys
         | Ty_Tuple                  => Ty_Tuple
         | Ty_Record                 => Ty_Record
         | Ty_Par                    => Ty_Par
         | Ty_Arrow                  => Ty_Arrow
         | Unparsed                  => Unparsed
        )
        (map unwrap $ Tree.children t)
    end

infix ==
fun n1 == n2 =
    let open Grammar in
      case (n1, n2) of
        (Exp_Var v1, Exp_Var v2) =>
        ValEnv.same (Variable.load v1) (Variable.load v2)
      | (Exp_SCon x, Exp_SCon y) => x = y
      | (Exp_Tuple, Exp_Tuple) => true
      | (Exp_List, Exp_List) => true
      | (Exp_Record, Exp_Record) => true
      | (Exp_Par, Exp_Par) => true
      | (Label_Plain x, Label_Plain y) => x = y
      | (Exp_App, Exp_App) => true
      | _ => false
    end

fun fail s = raise Fail ("NormalForm: " ^ s)

fun kappa p =
    let open Grammar in
      Tree.map
        (fn Pat_App => Exp_App
          | Pat_SCon x => Exp_SCon x
          | Pat_Tuple => Exp_Tuple
          | Pat_List => Exp_List
          | Pat_Record => Exp_Record
          | Pat_Par => Exp_Par
          | Pat_Var v => Exp_Var v
          | Label_Plain x => Label_Plain x
          | _ => fail "kappa"
        )
        p
    end

fun equiv (e1, e2) =
    let open Tree Grammar in
      (this e1) == (this e2) andalso
      List.all equiv $ ListPair.zip (children e1, children e2)
    end

fun FV p =
    Tree.fold (fn (n, vs) =>
                  let open Grammar
                    fun next v =
                        case Variable.load v of
                          (_, _, ValEnv.Val) => v :: vs
                        | _ => vs
                  in
                    case n of
                      Exp_Var v => next v
                    | Pat_Var v => next v
                    | Label_Short v => next v
                    | _ => vs
                  end) nil p

fun freshVar vs =
    let
      val cs = explode "xyzabcdefghijklmnopqrstuvw"
      val n = length cs
      val first = [0]
      val toString = implode o List.map (curry List.nth cs)
      fun next nil = [0]
        | next (c :: cs) =
          let
            val c' = c + 1
          in
            if c >= n then
              0 :: next cs
            else
              c' :: cs
          end
      fun loop cs =
          let
            val v = toString cs
          in
            if List.exists
                 (fn v' => Ident.toString (Variable.ident v') = v) vs then
              loop $ next cs
            else
              v
          end
      val v = loop first
      val id = Ident.fromString Fixity.Nonfix v
      val var = Variable.ofIdent id
      val vid = ValEnv.newVid ()
      val var = Variable.store var vid
    in
      var
    end

fun someVar p =
    let open Tree Grammar
      fun loop nil = NONE
        | loop (p :: ps) =
          case someVar p of
            SOME v => SOME v
          | NONE => loop ps
    in
      case this p of
        Pat_Var v =>
        (case Variable.load v of
           (_, _, ValEnv.Val) => SOME v
         | _ => loop $ children p
        )
      | _ => loop $ children p
    end

fun subs (e1, e2, e3) =
    let open Grammar
      exception Quit
      val vs = FV e2
      fun loop e =
          if equiv (e, e2) then
            e3
          else
            let
              fun maybeQuit v =
                  if List.exists
                       (fn v' => ValEnv.same
                                   (Variable.load v)
                                   (Variable.load v')) vs then
                    raise Quit
                  else
                    ()
              val _ =
                  case Tree.this e of
                    Exp_Var v => maybeQuit v
                  | Label_Short v => maybeQuit v
                  | _ => ()
            in
              Tree.join
                (Tree.this e)
                (map loop $ Tree.children e)
            end
    in
      SOME $ loop e1 handle Quit => NONE
    end

fun elimLayers (p, e) =
    let open Grammar Tree
      fun loop nil e = (nil, e)
        | loop (p :: ps) e =
          let
            val (p', e) = elimLayers (p, e)
            val (ps', e) = loop ps e
          in
            (p' :: ps', e)
          end
      val (ps, e) = loop (children p) e
    in
      case this p of
        Pat_Layered v =>
        let
          val p = hd ps
        in
          (p, valOf $ subs
                    (e,
                     Tree.singleton $ Exp_Var v,
                     join Exp_Par [kappa p]))
        end
      | n =>
        (join n ps, e)
    end

fun elimLists cons nill t =
    let open Grammar Tree
      fun braid (app, var, tuple) ts =
          let
            val nill = singleton $ var nill
            val cons = singleton $ var cons
            fun loop nil = nill
              | loop (t :: ts) =
                join app [cons, join tuple [t, loop ts]]
          in
            join Pat_Par [loop ts]
          end
      val ts = List.map (elimLists cons nill) $ children t
    in
      case this t of
        Exp_List => braid (Exp_App, Exp_Var, Exp_Tuple) ts
      | Pat_List => braid (Pat_App, Pat_Var, Pat_Tuple) ts
      | n => join n ts
    end

fun elimWildcards (p, e) =
    let open Grammar Tree
      val vs = ref (FV p @ FV e)
      fun new _ =
          let
            val v = freshVar (!vs)
          in
            vs := v :: !vs ;
            v
          end
    in
      (map
         (fn Pat_Wild => Pat_Var $ new ()
           | x => x)
         p,
       e)
    end

fun elimRecords (p, e) = (p, e)

fun elimUnit (p, e) = (p, e)

(* At this point we only concern ourselves with Pat_Var, Pat_Tuple Pat_App
 * Pat_SCon, Pat_Typed and Pat_Par
 *)

fun cover ps =
    let open Grammar Tree
      datatype pat = One
                   | Con of ValEnv.vid * pat
                   | Tup of pat list

      fun trans p =
          let
            exception Zero
            fun loop p =
                case this p of
                  Pat_Typed => loop $ hd $ children p
                | Pat_Par => loop $ hd $ children p
                | Pat_SCon _ => raise Zero
                | Pat_Tuple => Tup $ List.map loop $ children p
                | Pat_Wild => One
                | Pat_Var v =>
                  (case Variable.load v of
                     vid as (_, _, ValEnv.Con _) => Con (vid, One)
                   | (_, _, ValEnv.Val) => One
                   | (_, _, ValEnv.Exn) => raise Zero
                  )
                | Pat_App =>
                  (case children p of
                     [con, pat] =>
                     (case this con of
                        Pat_Var v =>
                        (case Variable.load v of
                           vid as (_, _, ValEnv.Con _) => Con (vid, loop pat)
                         | _ => raise Zero
                        )
                      | _ => fail "Illformed Pat_App"
                     )
                   | _ => fail "Illformed Pat_App"
                  )
                | _ => fail "Illformed pattern (eliminate lists, records and layers)"
          in
            SOME [loop p] handle Zero => NONE
          end

      fun loop (nil :: _) = true
        | loop nil = false
        | loop pss =
          let
            fun partition nil = (nil, nil, nil)
              | partition (ps :: pss) =
                let
                  val (ones, cons, tups) = partition pss
                in
                  case hd ps of
                    One => (ps :: ones, cons, tups)
                  | Con _ => (ones, ps :: cons, tups)
                  | Tup _ => (ones, cons, ps :: tups)
                end

            fun conLoop (ones, cons as (Con ((_, _, ValEnv.Con cs), _) :: _) :: _) =
                let
                  (* val _ = println "con" *)
                  val cons =
                      Dictionary.map
                        (fn c =>
                            List.mapPartial
                              (fn Con (c', p) :: ps =>
                                  if ValEnv.same c c' then
                                    SOME (p :: ps)
                                  else
                                    NONE
                                | _ => Crash.impossible "cover.con"
                              ) cons
                        ) cs
                in
                  Dictionary.all
                    (fn ps => loop (ones @ ps))
                    cons
                end
              | conLoop _ = Crash.impossible "cover.con"

            fun tupLoop (ones, tups as (Tup ps :: _) :: _) =
                let
                  (* val _ = println "tup" *)
                  val pad = length ps
                  val pss =
                      List.map
                        (fn One :: ps => List.tabulate (pad, const One) @ ps
                          | Tup ps :: ps' => ps @ ps'
                          | _ => Crash.impossible "cover.tup"
                        )
                        (ones @ tups)
                in
                  loop pss
                end
              | tupLoop _ = Crash.impossible "cover.tup"

            (* val _ = println "loop" *)
          in
            case partition pss of
              (ones, nil, nil) => loop $ List.map tl ones
            | (ones, cons, nil) => conLoop (ones, cons)
            | (ones, nil, tups) => tupLoop (ones, tups)
            | _ => fail "Can't mix constructors and tupples"
          end

    in
      loop $ List.mapPartial trans ps
    end

fun gen (p, e) =
    let open Grammar Tree
      fun loop nil e = (nil, e)
        | loop (p :: ps) e =
          let
            val (p', e) = gen (p, e)
            val (ps', e) = loop ps e
          in
            (p' :: ps', e)
          end
      val v = freshVar (FV p @ FV e)
    in
      case this p of
        Pat_Var _ => (p, e)
      | _ =>
        (case subs (e, kappa p, singleton $ Exp_Var v) of
           SOME e => (singleton $ Pat_Var v, e)
         | NONE =>
           let
             val (ps, e) = loop (children p) e
           in
             (join (this p) ps, e)
           end
        )
    end


fun cmp cmpvarid p1 p2 =
    let open Grammar Tree
      fun bypass p = hd $ children p
      fun vtos v = Ident.toString $ Variable.ident v
      fun cmpvar v1 v2 =
          case (Variable.load v1, Variable.load v2) of
            ((_, _, ValEnv.Val), (_, _, ValEnv.Val)) =>
            String.compare (vtos v1, vtos v2)
          | ((_, _, ValEnv.Val), _) => GREATER
          | (_, (_, _, ValEnv.Val)) => LESS
          | ((id1, _, _), (id2, _, _)) =>
            cmpvarid id1 id2
      val cmp = cmp cmpvarid
    in
      case (this p1, this p2) of
        (Pat_Par, _) => cmp (bypass p1) p2
      | (Pat_Typed, _) => cmp (bypass p1) p2
      | (_, Pat_Par) => cmp p1 (bypass p2)
      | (_, Pat_Typed) => cmp p1 (bypass p2)
      | (Pat_Var v1, Pat_Var v2) => cmpvar v1 v2
      | (Pat_Tuple, Pat_Tuple) =>
        List.collate
          (uncurry cmp)
          (children p1, children p2)
      | (Pat_App, Pat_App) =>
        List.collate
          (uncurry cmp)
          (children p1, children p2)
      | (Pat_Var _, _) => GREATER
      | (_, Pat_Var _) => LESS
      | _ => fail "Illformed pattern in totalcmp"
    end

fun totalcmp p1 p2 = cmp (curry Int.compare) p1 p2
fun partialcmp p1 p2 =
    let
      exception Unordered
      fun cmpvarid id1 id2 =
          if id1 = id2 then
            EQUAL
          else
            raise Unordered
    in
      SOME (cmp cmpvarid p1 p2) handle Unordered => NONE
    end

(* fun totalcmp (p1, p2) *)

(* fun partialcmp *)

(* fun convert t = *)
(*     case Wrap.unwrap $ Tree.this t of *)
(*       Exp_Var var => 7 *)
(*     | _           => 8 *)

end
