structure Infixing =
struct
   (* `fun' bindings are a pain in the posterior. The definition (V4, Apdx. B,
      Fig. 20) gives the syntax rules as a footnote (sigh). I've formalised
      them as below. The parser delivers a FUN binding as a sequence (>= 1)
      of atomic patterns, followed by an optional `: <ty>', and `=' and so on.
      Of that general syntax, I permit the following:

        (1) "fun" NonfixID NonfixAP+ (":" Ty)? "=" ...

        (2) "fun" "op" ID NonfixAP+ (":" Ty)? "=" ...

        (3) "fun" "(" NonfixAP InfixID NonfixAP ")" NonfixAP* (":" Ty)? "=" ...

        (4) "fun" NonfixAP InfixID NonfixAP (":" Ty)? "=" ...

      NonfixID is any identifier which isn't an infix. InfixID is an identifier
      with infix status. NonfixAP is any atomic pattern other than an isolated
      identifier which has infix status (that's legal in our parser). ID is
      any identifier (except "=" - look to Topdec_GRAM.src for the gory details
      of what we actually consider an identifier).
    *)

open SMLGrammar Fixity
infix or

fun die s = Crash.die "Infixing" s
val fail = Error.error

val join = Tree.join
val wrap = Wrap.wrap
val left = Wrap.left o Tree.this
val right = Wrap.right o Tree.this
val unwrap = Wrap.unwrap
val node = Wrap.unwrap o Tree.this
val children = Tree.children
val fixity = Ident.fixity o unwrap
val isInfix = Ident.isInfix o unwrap
val isNonfix = Ident.isNonfix o unwrap
val isQual = Ident.isQual o unwrap
val isUnqual = Ident.isUnqual o unwrap
val idToString = Ident.toString o unwrap

fun leftmost a b = Int.min (left a, left b)
fun rightmost a b = Int.max (right a, right b)

local
fun pair n a b = join (wrap n (left a) (right b)) [a, b]
fun toTree n i = join (wrap (n i) (Wrap.left i) (Wrap.right i)) nil
fun asId f t = (SOME o f o node) t handle General.Match => NONE
fun apply n a b = join (wrap n (leftmost a b) (rightmost a b)) [a, b]
in
structure Exp =
struct
type tree = tree
type ident = ident
val pair = pair Exp_Tuple
val toTree = toTree Exp_Var
val apply = apply Exp_App
val asId = asId (fn Exp_Var i => i | _ => raise General.Match)
end
structure ExpStack = InfixStack (Exp)

structure Pat =
struct
type tree = tree
type ident = ident
val pair = pair Pat_Tuple
val toTree = toTree Pat_Var
val apply = apply Pat_App
val asId = asId (fn Pat_Var i => i | _ => raise General.Match)
end
structure PatStack = InfixStack (Pat)
end (* end local *)

fun resolveArgs pats =
    let
      datatype category = Infixed of ident
                        | Other of tree

      fun category pat =
          case Pat.asId pat of
            SOME id =>
            if isInfix id then
              Infixed id
            else
              Other pat
          | NONE => Other pat

      val pair = Pat.pair
    in
      case map category pats of
        [Other a, Infixed id, Other b] => (* Success: Case 4 *)
        (id, [pair a b])
      | Other pat :: _ =>
        let
          val rest = List.tl pats
        in
          case node pat of
            Pat_Par =>
            (case map category (children pat) of
               [Other a, Infixed id, Other b] => (* Succes: Case 3 *)
               (id, Pat.pair a b :: rest)
             | _ => fail (left pat) "function name missing."
            )
          | Pat_Var id => (* Success: Case 1 or 2 *)
            (case (isNonfix id, isUnqual id, rest) of
               (true, true, _ :: _) => (id, rest)
             | (_, _, nil) => fail (left pat) "no argument(s) in function declaration."
             | (false, _, _) => fail (left pat) "identifier has infix status."
             | (_, false, _) => fail (left pat) "qualified identifier in declaration."
            )
          | _ => fail (left pat) "function name missing."
        end
      | Infixed id :: _ => fail (Wrap.left id) "put 'op' in front of identifier."
      | _ => die "resolveArgs"
    end

(* Takes a tree and a basis. Returns the new tree and the infixes declared by
   the tree *)
fun resolve (t, bas) =
    let
      fun ++ (a, b) = Dictionary.plus a b
      infix ++
      val empty = Dictionary.empty

      (* eta expansion to please the type system *)
      val fail = fn s => fail (left t) s

      val ts = children t

      fun run bas n =
          let
            fun loop (t :: ts, new) =
                let
                  val (t, new') = resolve (t, bas ++ new)
                  val (ts, new') = loop (ts, new ++ new')
                in
                  (t :: ts, new')
                end
              | loop x = x
            val (ts, new) = loop (ts, empty)
          in
            (join n ts, new)
          end
      fun continue _ = run bas (Tree.this t)

      fun localize _ =
          case children t of
            [a, b] =>
            let
              val (a', new) = resolve (a, bas)
              val (b', new) = resolve (b, bas ++ new)
            in
              (join (Tree.this t) [a', b'], new)
            end
          | _ => fail "Must have excatly two children in localization."

      fun correct n ts = (join (wrap n (left t) (right t)) ts, empty)

      fun update id f =
          let
            val id' = unwrap id
          in
            case Ident.fixity id' of
              Op => id
            | _ => wrap (Ident.setFixity id' f) (Wrap.left id) (Wrap.right id)
          end
      fun skip _ = (t, empty)

      fun correctVar n id =
          case Dictionary.lookup bas (idToString id) of
            SOME f => correct (n (update id f)) ts
          | NONE => skip ()

      fun resolveApp resolve =
          let
            val (t, _) = continue ()
          in
            (resolve (children t), empty)
          end
    in
      case node t of
        Topdecs => continue ()
      | Strdecs => continue ()
      | Strdec_Str => continue ()
      | Strdec_Local => localize ()
      | Sigdec_Sig => skip ()
      | Fundec_Fun => continue ()
      | Strbind _ => continue ()
      | Sigbind _ => continue ()
      | Funbind _ => continue ()
      | Funarg_Structure _ => skip ()
      | Funarg_Spec => skip ()
      | Strexp_Struct =>
        let
          val (t, _) = continue ()
        in
          (t, empty)
        end
      | Strexp_Let => localize ()
      | Strexp_Con => continue ()
      | Strexp_Fun _ => continue ()
      | Strexp_Var _ => skip ()
      | Sigcon _ => skip ()
      | Sigexp_Where => skip ()
      | Sigexp_Spec => skip ()
      | Sigexp_Var _ => skip ()
      | Wherespecs => skip ()
      | Wherespec _ => skip ()
      | Spec_Val => skip ()
      | Spec_Type => skip ()
      | Spec_Typedef => skip ()
      | Spec_EqType => skip ()
      | Spec_Datatype => skip ()
      | Spec_Replication _ => skip ()
      | Spec_Exception => skip ()
      | Spec_Structure => skip ()
      | Spec_Include => skip ()
      | Spec_IncludeSigids _ => skip ()
      | Spec_Sharing _ => skip ()
      | Spec_SharingStructure _ => skip ()
      | Strdesc _ => skip ()
      | Tydesc _ => skip ()
      | Valdesc _ => skip ()
      | Exndesc _ => skip ()
      | Datatypes => skip ()
      | Datatype _ => skip ()
      | Constructor _ => skip ()
      | Replication _ => skip ()
      | MaybeTy => skip ()
      | Decs => continue ()
      | Dec_Local => localize ()
      | Dec_Val _ => continue ()
      | Dec_Fun _ => continue ()
      | Dec_Type => skip ()
      | Dec_Datatype => skip ()
      | Dec_Replication _ => skip ()
      | Dec_Abstype => continue ()
      | Dec_Exception => skip ()
      | Dec_Open _ => skip ()
      | Dec_Fix (f, ids) =>
        let
          val new =
              foldl
                (fn (id, new) =>
                    let
                      val id = unwrap id
                    in
                      if Ident.isUnqual id then
                        Dictionary.update new (Ident.toString id, f)
                      else
                        fail "Identifier must be unqualified."
                    end
                ) empty ids
        in
          (t, new)
        end
      | Dec_Overload _ => skip ()
      | Valbinds => continue ()
      | Valbind_Plain => continue ()
      | Valbind_Rec => continue ()
      | Match => continue ()
      | Clause _ => die "Encountered Clause in resolve."
      | FlatClause =>
        let
          val (t, _) = continue ()
        in
          case children t of
            pats :: rest =>
            let
              val (id, pats') = resolveArgs (children pats)
            in
              correct (Clause id) (join (Tree.this pats) pats' :: rest)
            end
          | _ => die "No patterns in FlatClause."
        end
      | Rule => continue ()
      | Datbinds => skip ()
      | Withtypes => skip ()
      | Tybind _ => skip ()
      | Exps => continue ()
      | Exp_Handle => continue ()
      | Exp_Orelse => continue ()
      | Exp_Andalso => continue ()
      | Exp_Typed => continue ()
      | Exp_App => continue ()
      | Exp_FlatApp => resolveApp ExpStack.resolve
      | Exp_Fn => continue ()
      | Exp_Case => continue ()
      | Exp_While => continue ()
      | Exp_If => continue ()
      | Exp_Raise => continue ()
      | Exp_Var id => correctVar Exp_Var id
      | Exp_SCon _ => skip ()
      | Exp_Selector _ => skip ()
      | Exp_Record => continue ()
      | Exp_Unit => skip ()
      | Exp_Par => continue ()
      | Exp_Seq => continue ()
      | Exp_Tuple => continue ()
      | Exp_List => continue ()
      | Exp_Let => localize ()
      | Exp_LetSeq => localize ()
      | Label_Plain _ => continue ()
      | Label_Short _ => continue ()
      | MaybePat => continue ()
      | Pats => continue ()
      | Pat_Layered _ => continue ()
      | Pat_Typed => continue ()
      | Pat_App => continue ()
      | Pat_FlatApp => resolveApp PatStack.resolve
      | Pat_Var id => correctVar Pat_Var id
      | Pat_SCon _ => skip ()
      | Pat_Wild => skip ()
      | Pat_Tuple => continue ()
      | Pat_List => continue ()
      | Pat_Record => continue ()
      | Pat_FlexibleRecord => continue ()
      | Pat_Par => continue ()
      | Tys => skip ()
      | Ty_Tuple => skip ()
      | Ty_Record => skip ()
      | Ty_Var _ => skip ()
      | Ty_Con _ => skip ()
      | Ty_Par => skip ()
      | Ty_Arrow => skip ()
      | Unparsed => skip ()
    end
end
