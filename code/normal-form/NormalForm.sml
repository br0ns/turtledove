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

fun fail s = raise Fail ("NormalForm: " ^ s)
fun fail' s t =
    let open Layout infix \
      val t = Grammar.showUnwrapped NONE t
    in
      println NONE (txt s \ indent 2 t) ;
      raise Fail "NormalForm"
    end

fun showClause (ps, e) =
    let open Layout infix \ ++ in
      hsep (map PPGrammar.showUnwrapped ps) ++ eq ++ PPGrammar.showUnwrapped e
    end
fun showClauses cs =
    Layout.vsep $ map showClause cs

(* structure Ord = *)
(* struct *)
(* type t = int *)
(* fun compare (x, _, _) (y, _, _) = Int.compare (x, y) *)
(* end *)

(* structure Map = OrderedMap (Ord) *)
(* structure Set = OrderedSet (Ord) *)

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
          | _ => fail' "kappa" p
        )
        p
    end

fun FV ts =
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
                  end) nil
              (* dummy node *)
              $ Tree.join Grammar.Pat_Tuple ts

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

fun equiv (e1, e2) =
    let open Tree Grammar
      fun bypass t = hd $ children t
    in
      case (this e1, this e2) of
        (Exp_Par, _) => equiv (bypass e1, e2)
      | (_, Exp_Par) => equiv (e1, bypass e2)
      | (Exp_Typed, _) => equiv (bypass e1, e2)
      | (_, Exp_Typed) => equiv (e1, bypass e2)
      | (n1, n2) =>
        n1 == n2 andalso
        List.all equiv $ ListPair.zip (children e1, children e2)
    end

fun subs (e1, e2, e3) =
    let open Grammar
      exception Quit
      val vs = FV [e2]
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

fun elimShortLabels (ps, e) =
    let open Grammar Tree
      fun one p =
          let
            val ps = many $ children p
          in
            case this p of
              Label_Short v =>
              join
                (Label_Plain $ Variable.ident v)
                [case List.map children $ children p of
                   [top, pop] =>
                   let
                     val p =
                         case pop of
                           [pat] => join (Pat_Layered v) [pat]
                         | _ => singleton $ Pat_Var v
                   in
                     case top of
                       [ty] => join Pat_Typed [p, ty]
                     | _ => p
                   end
                 | _ => fail "Illformed Label_Plain"
                ]
            | n => join n ps
          end
      and many ps = List.map one ps
    in
      (many ps, e)
    end

fun elimLayers (ps, e) =
    let open Grammar Tree
      fun one (p, e) =
          case this p of
            Pat_Layered v =>
            let
              val p = hd $ children p
            in
              (p, valOf $ subs
                        (e,
                         Tree.singleton $ Exp_Var v,
                         join Exp_Par [kappa p]))
            end
          | n => Arrow.first (join n) $ many (children p, e)
      and many (nil, e) = (nil, e)
        | many (p :: ps, e) =
          let
            val (p', e) = one (p, e)
            val (ps', e) = many (ps, e)
          in
            (p' :: ps', e)
          end
    in
      many (ps, e)
    end

fun elimLists cons nill (ps, e) =
    let open Grammar Tree
      fun braid (app, var, tuple, par) ts =
          let
            val nill = singleton $ var nill
            val cons = singleton $ var cons
            fun loop nil = nill
              | loop (t :: ts) =
                join app [cons, join tuple [t, loop ts]]
          in
            join par [loop ts]
          end
      fun one t =
          let
            val ts = many $ children t
          in
            case this t of
              Exp_List => braid (Exp_App, Exp_Var, Exp_Tuple, Exp_Par) ts
            | Pat_List => braid (Pat_App, Pat_Var, Pat_Tuple, Pat_Par) ts
            | n => join n ts
          end
      and many ts = List.map one ts
    in
      (many ps, one e)
    end

fun elimWildcards (ps, e) =
    let open Grammar Tree
      val vs = ref $ FV (e :: ps)
      fun new _ =
          let
            val v = freshVar (!vs)
          in
            vs := v :: !vs ;
            v
          end
    in
      (List.map
         (map
            (fn Pat_Wild => Pat_Var $ new ()
              | x => x)
         ) ps,
       e)
    end

(* At this point we only concern ourselves with Pat_Var, Pat_Tuple Pat_App
 * Pat_SCon, Pat_Typed and Pat_Par
 *)

fun cover pss =
    let open Grammar Tree
      datatype pat = One
                   | Con of ValEnv.vid * pat
                   | Tup of pat list

      fun trans ps =
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
            SOME $ List.map loop ps handle Zero => NONE
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
      loop $ List.mapPartial trans pss
    end

fun generalise (ps, e) =
    let open Grammar Tree
      fun disjoint xs ys =
          List.all
            (fn y =>
                List.all
                  (fn x =>
                      not $ ValEnv.same (Variable.load x) (Variable.load y)
                  )
                  xs
            )
            ys
      val vs = ref $ FV (e :: ps)
      fun new _ =
          let
            val v = freshVar (!vs)
          in
            vs := v :: !vs ;
            v
          end
      fun isVar p =
          case this p of
            Pat_Var v =>
            (case Variable.load v of
               (_, _, ValEnv.Val) => true
             | _ => false)
          | _ => false
      fun one (p, e) =
          if isVar p then
            (p, e)
          else
            let
              val v = new ()
            in
              (* if disjoint (FV ps) (FV [e]) then *)
              (*   (singleton $ Pat_Var v, e) *)
              (* else *)
                case subs (e, kappa p, singleton $ Exp_Var v) of
                  SOME e => (singleton $ Pat_Var v, e)
                | NONE =>
                  case this p of
                    Pat_App =>
                    (case children p of
                       [p1, p2] =>
                       let
                         val (p2, e) = one (p2, e)
                       in
                         (join Pat_App [p1, p2], e)
                       end
                     | _ => fail "Illformed Pat_App"
                    )
                  | n => Arrow.first (join n) $ many (children p, e)
            end
      and many (nil, e) = (nil, e)
        | many (p :: ps, e) =
          let
            val (p', e) = one (p, e)
            val (ps', e) = many (ps, e)
          in
            (p' :: ps', e)
          end
    in
      many (ps, e)
    end

fun totalcmp ps1 ps2 =
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
            Int.compare (id1, id2)

      fun one (p1, p2) =
          case (this p1, this p2) of
            (Pat_Par, _) => one (bypass p1, p2)
          | (Pat_Typed, _) => one (bypass p1, p2)
          | (_, Pat_Par) => one (p1, bypass p2)
          | (_, Pat_Typed) => one (p1, bypass p2)
          | (Pat_Var v1, Pat_Var v2) => cmpvar v1 v2
          | (Pat_Tuple, Pat_Tuple) =>
            many (children p1, children p2)
          | (Pat_App, Pat_App) =>
            many (children p1, children p2)
          | (Pat_Var _, _) => GREATER
          | (_, Pat_Var _) => LESS
          | _ => fail "Illformed pattern in totalcmp"

      and many (ps1, ps2) =
          List.collate one (ps1, ps2)
    in
      many (ps1, ps2)
    end

fun partialcmp ps1 ps2 =
    let open Grammar Tree
      exception Unordered
      fun bypass p = hd $ children p
      fun vtos v = Ident.toString $ Variable.ident v
      fun cmpvar v1 v2 =
          case (Variable.load v1, Variable.load v2) of
            ((_, _, ValEnv.Val), (_, _, ValEnv.Val)) =>
            EQUAL
          | ((_, _, ValEnv.Val), _) => GREATER
          | (_, (_, _, ValEnv.Val)) => LESS
          | ((id1, _, _), (id2, _, _)) =>
            if id1 = id2 then
              EQUAL
            else
              raise Unordered

      fun isCon v =
          case Variable.load v of
            (_, _, ValEnv.Val) => false
          | _ => true

      fun one (p1, p2) =
          case (this p1, this p2) of
            (Pat_Par, _) => one (bypass p1, p2)
          | (Pat_Typed, _) => one (bypass p1, p2)
          | (_, Pat_Par) => one (p1, bypass p2)
          | (_, Pat_Typed) => one (p1, bypass p2)
          | (Pat_Var v1, Pat_Var v2) => cmpvar v1 v2
          | (Pat_Tuple, Pat_Tuple) =>
            many (children p1, children p2)
          | (Pat_App, Pat_Var v) =>
            if isCon v then
              raise Unordered
            else
              LESS
          | (Pat_Var v, Pat_App) =>
            if isCon v then
              raise Unordered
            else
              GREATER
          | (Pat_App, Pat_App) =>
            many (children p1, children p2)
          | (Pat_Var _, _) => GREATER
          | (_, Pat_Var _) => LESS
          | _ => fail "Illformed pattern in totalcmp"

      and many (ps1, ps2) =
          List.foldl
            (fn (GREATER, LESS) => raise Unordered
              | (LESS, GREATER) => raise Unordered
              | (EQUAL, x) => x
              | (GREATER, _) => GREATER
              | (LESS, _) => LESS
            )
            EQUAL
            $ List.map one $ ListPair.zip (ps1, ps2)
    in
      SOME $ many (ps1, ps2) handle Unordered => NONE
    end

fun convert basis cs =
    let open Grammar Tree
      fun bypass p = hd $ children p

      fun eqpat ps1 ps2 =
          let
            fun eqvar v1 v2 =
                case (Variable.load v1, Variable.load v2) of
                  ((id1, _, ValEnv.Val), (id2, _, ValEnv.Val)) =>
                  SOME $ IntMap.singleton (id1, id2)
                | (v1, v2) =>
                  if ValEnv.same v1 v2 then
                    SOME IntMap.empty
                  else
                    NONE

            fun one (p1, p2) =
                case (this p1, this p2) of
                  (Pat_Par, _) => one (bypass p1, p2)
                | (Pat_Typed, _) => one (bypass p1, p2)
                | (_, Pat_Par) => one (p1, bypass p2)
                | (_, Pat_Typed) => one (p1, bypass p2)
                | (Pat_Var v1, Pat_Var v2) => eqvar v1 v2
                | (Pat_Tuple, Pat_Tuple) => many (children p1, children p2)
                | (Pat_App, Pat_App) => many (children p1, children p2)
                | _ => NONE

            and many (nil, nil) = SOME IntMap.empty
              | many (p1 :: ps1, p2 :: ps2) =
                (case one (p1, p2) of
                   SOME m => Option.map (IntMap.plus m) $ many (ps1, ps2)
                 | NONE => NONE)
              | many _ = NONE
          in
            many (ps1, ps2)
          end

      fun eqexp m e1 e2 =
          let
            exception Unequal
            fun eqvar v1 v2 =
                case (Variable.load v1, Variable.load v2) of
                  ((id1, _, ValEnv.Val), (id2, _, ValEnv.Val)) =>
                  (case IntMap.lookup m id1 of
                     SOME id1' => id1' = id2
                   | NONE => id1 = id2)
                | (v1, v2) => ValEnv.same v1 v2
            fun next _ =
                List.all (uncurry $ eqexp m)
                         $ ListPair.zip (children e1, children e2)
          in
            case (this e1, this e2) of
              (Exp_Par, _) => eqexp m (bypass e1) e2
            | (Exp_Typed, _) => eqexp m (bypass e1) e2
            | (_, Exp_Par) => eqexp m e1 (bypass e2)
            | (_, Exp_Typed) => eqexp m e1 (bypass e2)
            | (Exp_Var v1, Exp_Var v2) => eqvar v1 v2
            | (Rule, Rule) =>
              (* Children are allready in normal form *)
              (case (children e1, children e2) of
                 ([p1, e1], [p2, e2]) =>
                 (case eqpat [p1] [p2] of
                    SOME m' => eqexp (IntMap.plus m m') e2 e2
                  | NONE => false)
               | _ => fail "Illformed value Rule"
              )
            | (Exp_Handle, Exp_Handle) => next ()
            | (Exp_Orelse, Exp_Orelse) => next ()
            | (Exp_Andalso, Exp_Andalso) => next ()
            | (Exp_App, Exp_App) => next ()
            | (Exp_Fn, Exp_Fn) => next ()
            | (Exp_Case, Exp_Case) => next ()
            | (Exp_While, Exp_While) => next ()
            | (Exp_If, Exp_If) => next ()
            | (Exp_Raise, Exp_Raise) => next ()
            | (Exp_SCon x, Exp_SCon y) => x = y
            | (Exp_Seq, Exp_Seq) => next ()
            | (Exp_Tuple, Exp_Tuple) => next ()
            | _ => false
          end

      fun eq (ps1, e1) (ps2, e2) =
          case eqpat ps1 ps2 of
            SOME m => eqexp m e1 e2
          | NONE => false

      fun shadowed (ps, _) cs =
          List.exists
            (fn (ps', _) =>
                case partialcmp ps' ps of
                  SOME GREATER => true
                | _ => false)
            cs

      val cover = cover o List.map fst

      fun clauses s cs =
          let open Layout infix \
          in
            println NONE (txt s \ indent 2 (showClauses cs))
          end
      fun clause s c =
          let open Layout infix \
          in
            println NONE (txt s \ indent 2 (showClause c))
          end

      fun elim cs =
          let
            fun loop nil cs' = cs'
              | loop (c :: cs) cs' =
                if cover cs' then
                  ((Flags.get "Verbose" andalso
                   (clauses "These are a cover:" $ rev cs' ;
                    clauses "Dropping:" $ rev (c :: cs) ;
                    true));
                   cs'
                  )
                else if shadowed c cs' then
                  ((Flags.get "Verbose" andalso
                    (clause "This is shadowed:" c ;
                    true));
                   loop cs cs'
                  )
                else
                  loop cs (c :: cs')
            val cs = rev $ loop cs nil
          in
            (Flags.get "Verbose" andalso
             (clauses "After elimination:" cs;
              true));
             cs
          end

      fun gen nil = nil
        | gen ((c as (ps, e)) :: cs) =
          let
            exception Next
            val c' as (ps', e') = generalise c
            fun loop nil (csa, csb) = rev csa @ c' :: rev csb
              | loop ((c'' as (ps'', e'')) :: cs) (csa, csb) =
                case (partialcmp ps ps'', partialcmp ps' ps'') of
                  (* Case 1 *)
                  (_, SOME EQUAL) =>
                  if eq c' c'' then
                    ((Flags.get "Verbose" andalso
                      (clause "Deleted through unification:" c'' ;
                       true));
                     loop cs (csa, csb)
                    )
                  else
                    raise Next
                (* Case 2a *)
                | (NONE, NONE) => loop cs (csa, c'' :: csb)
                (* Case 2b *)
                | (SOME LESS, SOME LESS) => loop cs (csa, c'' :: csb)
                (* Case 3 *)
                | (NONE, SOME GREATER) => loop cs (c'' :: csa, csb)
                (* Case 4 *)
                | (SOME LESS, SOME GREATER) => raise Next
                (* Impossible *)
                | x => raise Next before
                             println
                               $ Show.pair (Show.option Show.order)
                               (Show.option Show.order)
                               x
            val cs = gen cs
            val _ = Flags.get "Verbose" andalso
                      (clause "Generalising" c;
                       clause "becomes" c';
                       true)
            val cs = loop cs (nil, nil) handle Next => c :: cs
            val _ = Flags.get "Verbose" andalso
                    (clauses "Now have" cs;
                     println "";
                     true)
          in
            cs
          end

      val sort = List.sort (fn (ps1, _) => fn (ps2, _) => totalcmp ps1 ps2)

      exception NonExhaustive

      fun checkCover cs =
          if cover cs then
            cs
          else
            raise NonExhaustive before clauses "Match is not exhaustive" cs

      fun var {environment, interface, infixing} name =
          let
            val id = Ident.fromString Fixity.Nonfix name
            val id =
                if Ident.isUnqual id then
                  case Dictionary.lookup infixing name of
                    SOME fixity => Ident.setFixity id fixity
                  | NONE => id
                else
                  id
            val vid = ValEnv.findVal environment id
            val var = Variable.ofIdent id
            val var = Variable.store var vid
          in
            var
          end
      val cons = var basis "::"
      val nill = var basis "nil"
    in
      (
       (Flags.get "Verbose" andalso
        (clauses "Normalizing" cs; true)) ;
       gen $ sort $ elim $ checkCover $
           List.map
           (elimLists cons nill o
            elimLayers o
            elimWildcards o
            elimShortLabels)
           cs
      ) handle NonExhaustive => cs
    end

fun normalize basis t =
    let open Grammar Tree
      fun extract nil = (Rule, nil)
        | extract (t :: ts) =
          let
            val (_, cs) = extract ts
          in
            case (this t, children t) of
              (Rule, [p, e]) => (Rule, ([p], e) :: cs)
            | (Clause v, [ps, top, e]) => (Clause v, (children ps, e) :: cs)
            | _ => fail "Illformed Match"
          end
      fun inject (n, cs) =
          case n of
            Rule =>
            List.map (fn (ps, e) => join n [hd ps, e]) cs
          | Clause _ =>
            List.map (fn (ps, e) => join n [join Pats ps, join MaybeTy [], e]) cs
          | _ => Crash.impossible "normalize.inject"
      val ts = List.map (normalize basis) $ children t
    in
      case (this t) of
        Match =>
        let
          val (n, cs) = extract ts
          val cs' = convert basis cs
          val ts' = inject (n, cs')
        in
          join Match ts'
        end
      | n => join n ts
    end

end
