structure Environment =
struct
exception Error of int * string

(* This is based on the dynamic semantics of SML. In some sense this is a
 * simplification i since we don't exactly evaluate every expression but we do
 * compute an environment. Instead of values we use internal unique id's.
 * An environment (or basis in the terminology of the definition) consists of a
 * value environment and an interface environment. Interfaces are stripped down
 * signatures. They arise from signatures and functors and they can only exists
 * on the outermost level.
 * The value environment describe the bound identifiers. An identifier can be
 * bound to one of four things:
 *   (1) A value. We special handle two cases:
 *       (1.1) The identifier is bound to another identifier: We replicate the
 *             internal unique id.
 *       (1.2) The identfier is bound to a special constant: We bind the
 *             identifier to the SCon instead of an internal id.
 *   (2) A datatype constructor. Along with each constructor we store the set of
 *       constructors belonging to its datatype.
 *   (3) A structure. A structure is itself an environment.
 * Because of datatype replication we also store the datatypes.
 *
 * Even though internal ids are meaningless for interfaces (they corrospons to
 * values) we use value environments to represent interfaces because this makes
 * the code much simpler.
 *)

open Grammar

val this = Tree.this
val join = Tree.join
val left = Wrap.left
val right = Wrap.right
val children = Tree.children
fun wrap n l r = Wrap.extend
                   (fn p => fn e => {position = p, environment = e}) n l r
val unwrap = Wrap.unwrap
fun node t = unwrap $ this t

val empty = ValEnv.empty

val rec resolve :
    (ident, variable, int) ast * IntEnv.t * ValEnv.t ->
    (ident, variable, {position: int, environment: ValEnv.t}) ast * IntEnv.t * ValEnv.t
  =
fn (t, int, env) =>
    let
      fun fail s = raise Error (left $ this t, s)

      infix ++
      val op++ = ValEnv.++

      fun run node base =
          let
            fun loop (nil, int, env') = (nil, int, env')
              | loop (t :: ts, int, env') =
                let
                  val (t', int, env'') = resolve (t, int, env ++ env')
                  val (ts', int, env'') = loop (ts, int, env' ++ env'')
                in
                  (t' :: ts', int, env'')
                end
            val (ts, int, env') = loop (children t, int, base)
          in
            (join
               (wrap
                  node
                  env
                  env'
               ) ts,
             int,
             env')
          end

      fun continue _ = run (this t) empty

      fun newNode node =
          let
            val n = this t
            val (l, r) = (left n, right n)
          in
            run (Wrap.wrap node l r) empty
          end

      fun localize _ =
          case children t of
            [a, b] =>
            let
              val (a', int, env') = resolve (a, int, env)
              val (b', int, env') = resolve (b, int, env ++ env')
            in
              (join
                 (wrap
                    (this t)
                    env
                    env'
                 ) [a', b'],
               int,
               env')
            end
          | _ => fail "Must have excatly two children in localization."

      fun new bind id =
          let
            val (t', int, env') = continue ()
            val env' = bind (id, env')
          in
            (join
               (wrap
                  (* Notice t, not t' *)
                  (this t)
                  env
                  env'
               )
               (children t'),
             int,
             env'
            )
          end

      fun rep bind (id1, id2) =
          let
            val env' = bind env (id1, id2)
          in
            (join
               (wrap
                  (this t)
                  env
                  env')
               nil,
             int,
             env'
            )
          end

      val newStr = new ValEnv.newStr
      val newDat = new ValEnv.newDat
      val newVal = new (ValEnv.newVal o fst)
      val newExn = new (ValEnv.newExn o fst)
      val exnRep = rep ValEnv.exnRep
      val datRep = rep ValEnv.datRep

      fun ihatevaluedeclarations _ =
          let
            val (pats, exps) =
                ListPair.unzip
                  $ map
                  (fn t =>
                      case children t of
                        [pat, exp] => (pat, exp)
                      | _          => fail "Illformed value binding"
                  )
                  (children t)
            val recursive =
                case node $ hd $ children t of
                  Valbind_Rec => true
                | _ => false

            val (pats, envs) =
                ListPair.unzip
                  $ map
                  (fn pat =>
                      let
                        val (pat', _, env') = resolve (pat, int, env)
                      in
                        (pat', env')
                      end
                  )
                  pats

            fun zip (node :: nodes,
                     pat :: pats,
                     exp :: exps,
                     env :: envs) =
                (node, pat, exp, env) ::
                zip (nodes, pats, exps, envs)
              | zip _ = nil

            val recenv =
                List.foldl
                  (fn (env', env) => env ++ env')
                  empty
                  envs

            fun loop env' ((node, pat, exp, env'') :: xs) =
                let
                  val (exp', _, _) = resolve (exp, int, env')
                  val node =
                      join
                        (wrap
                           (this node)
                           env
                           env''
                        )
                        [pat, exp']
                  val nodes = loop
                                (if recursive then
                                   env'
                                 else
                                   env' ++ env'')
                                xs
                in
                  node :: nodes
                end
              | loop _ _ = nil

            val nodes =
                loop
                  (if recursive then
                     env ++ recenv
                   else
                     env)
                  $ zip (children t, pats, exps, envs)
          in
            (join
               (wrap
                  (this t)
                  env
                  recenv
               )
               nodes,
             int,
             recenv)
          end

      fun ihatefunctiondeclarations _ =
          let
            fun idOfMatch m =
                case node $ hd $ children m of
                  Clause var => Variable.ident $ unwrap var
                | _ => fail "Illformed match"
            val ids = map idOfMatch $ children t

            val env' =
                List.foldl
                  (fn (id, env) => env ++ ValEnv.newVal id)
                  empty
                  ids

          in
            run (this t) env'
          end

      fun ihatedatatypedeclarations did =
          let
            fun idOfCon c =
                case node c of
                  Constructor var => Variable.ident $ unwrap var
                | _ => fail "Illformed datatype"
            val ids = map idOfCon $ children t

            val env' =
                List.foldl
                  (fn (id, env) => env ++ ValEnv.newVal id)
                  empty
                  ids
          in
            run (this t) $ ValEnv.newDat (did, env')
          end
    in
      (* continue () *)

      case node t of
        Rule_Rules => continue ()
      | Rule_Type_Clauses _ => continue ()
      | Rule_Type_Expressions _ => continue ()
      | Rule_Scheme => continue ()
      | Rule_Clauses => continue ()
      | Rule_Clause => continue ()
      | Rule_Cstrns => continue ()
      | Rule_Cstrn_Rel _ => continue ()
      | Rule_Trans _ => continue ()
      | Rule_Meta_Pat _ => continue ()
      | Rule_Self => continue ()

      | Topdecs => continue ()
      | Strdecs => continue ()
      | Strdec_Str => continue ()
      | Strdec_Local => localize ()
      | Sigdec_Sig => continue ()
      | Fundec_Fun => continue ()
      | Strbind strid =>
        (case children t of
           [strexp, sigcon] =>
           let
             val (strexp, int, env') = resolve (strexp, int, env)
             val (sigcon, int, int') = resolve (sigcon, int, env)
             (* val _ = println "baz" *)
             val env' = case children sigcon of
                          nil => env'
                        | _   => ValEnv.constrain env' int'
             (* val _ = println "baz'" *)
           in
             (join
                (wrap
                   (this t)
                   env
                   env'
                ) [strexp, sigcon],
              int,
              ValEnv.newStr (unwrap strid, env')
             )
           end
         | _ => fail "Illformed Strbind")
      | Sigbind sigid =>
        let
          (* This here is what we call a hack: We regard signatures as a kind of
           * structure, so we can compute an environment from them. Then we
           * store that environment as an interface.
           *)
          val (t', int, env') = continue ()
        in
          (t',
           IntEnv.bindSig int (unwrap sigid, env'),
           empty
          )
        end
      | Funbind funid =>
        (case children t of
           [funarg, strexp, sigcon] =>
           let
             val (funarg', _, argint) = resolve (funarg, int, env)

             val argname =
                 case node funarg of
                   Funarg_Structure strid => SOME $ unwrap strid
                 | _                      => NONE

             val argint' =
                 case argname of
                   SOME strid => ValEnv.newStr (strid, argint)
                 | NONE       => argint

             val (strexp', _, _) = resolve (strexp, int, env ++ argint')
             val (sigcon', _, resint) = resolve (sigcon, int, env ++ argint')
             val resint = case children sigcon of
                            nil => NONE
                          | _   => SOME resint
           in
             (join
                (wrap
                   (this t)
                   env
                   empty
                )
                [funarg', strexp', sigcon'],
              IntEnv.bindFun int (unwrap funid, (strexp, argname, argint, resint, int, env)),
              empty
             )
           end
         | _ => fail "Illformed Funbind"
        )
      | Funarg_Structure strid => continue ()
      | Funarg_Spec => continue ()
      | Strexp_Struct => continue ()
      | Strexp_Let => localize ()
      | Strexp_Con => continue ()
      | Strexp_Fun fid =>
        (case children t of
           [arg] =>
           let
             val (strexp, argname, argint, resint, fint, fenv) = IntEnv.findFun int (unwrap fid)
             val (arg', int, argenv) = resolve (arg, int, env)

             (* val _ = println "foo" *)
             (* val _ = println $ Ident.toString $ unwrap fid *)
             (* val _ = println "Arg:" *)
             (* val _ = Layout.println NONE $ ValEnv.show argenv *)
             (* val _ = println "Int:" *)
             (* val _ = Layout.println NONE $ ValEnv.show argint *)
             val argenv = ValEnv.constrain argenv argint
             (* val _ = println "foo'" *)
             val argenv = case argname of
                            SOME strid => ValEnv.newStr (strid, argenv)
                          | NONE       => argenv
             val (strexp', _, env') = resolve (strexp, fint, fenv ++ argenv)
             (* val _ = println "bar" *)
             val env' = case resint of
                          SOME int => ValEnv.constrain env' int
                        | NONE     => env'
             (* val _ = println "bar'" *)
           in
             (join
                (wrap
                   (this t)
                   env
                   env'
                )
                [arg'],
              int,
              env'
             )
           end
         | _ => fail "Illformed Strexp_Fun"
        )
      | Strexp_Var strid =>
        let
          val (t', int, _) = continue ()
        in
          (t',
           int,
           ValEnv.findStr env $ unwrap strid
          )
        end
      | Sigcon _ => continue ()
      | Sigexp_Where => continue ()
      | Sigexp_Spec => continue ()
      | Sigexp_Var sigid =>
        let
          val (t', int, _) = continue ()
        in
          (t',
           int,
           IntEnv.findSig int $ unwrap sigid
          )
        end
      | Wherespecs => continue ()
      | Wherespec _ => continue ()
      | Spec_Val => continue ()
      | Spec_Type => continue ()
      | Spec_Typedef => continue ()
      | Spec_EqType => continue ()
      | Spec_Datatype => continue ()
      | Spec_Replication (dat1, dat2) =>
        datRep (unwrap dat1, unwrap dat2)
      | Spec_Exception => continue ()
      | Spec_Structure => continue ()
      | Spec_Include => continue ()
      | Spec_IncludeSigids sigids =>
        let
          val (t', int, _) = continue ()
        in
          (t',
           int,
           List.foldl
             (fn (id, env) =>
                 env ++ IntEnv.findSig int id
             )
             empty
             (map unwrap sigids)
          )
        end
      | Spec_Sharing _ => continue ()
      | Spec_SharingStructure _ => continue ()
      | Strdesc strid =>
        newStr $ unwrap strid
      | Tydesc _ => continue ()
      | Valdesc id =>
        newVal $ unwrap id
      | Exndesc eid =>
        newExn $ unwrap eid
      | Datatypes => continue ()
      | Datatype (_, did) => ihatedatatypedeclarations $ unwrap did
      | Constructor var =>
        let
          val id = Variable.ident $ unwrap var
        in
          if ValEnv.isCon env id then
            let
              val vid = ValEnv.findVal env id
              val var = Wrap.modify (fn var => Variable.store var vid) var
            in
              newNode $ Constructor var
            end
          else
            let
              val (t', int, env') = newExn id
              val vid = ValEnv.findVal env' id
              val var = Wrap.modify (fn var => Variable.store var vid) var
              val node =
                  Wrap.modify (const $ Constructor var) (this t')
            in
              (join node $ children t', int, env')
            end
        end
      | Replication (exn1, exn2) =>
        let
          val (id1, id2) =
              (Variable.ident $ unwrap exn1, Variable.ident $ unwrap exn2)
          val (t', int, env') =
              exnRep (id1, id2)
          val (vid1, vid2) =
              (ValEnv.findVal env' id1,
               ValEnv.findVal env id2)
          val (exn1, exn2) =
              (Wrap.modify (fn var => Variable.store var vid1) exn1,
               Wrap.modify (fn var => Variable.store var vid2) exn2)
          val t' =
              join
                (Wrap.modify
                   (const $ Replication (exn1, exn2))
                   (this t')
                )
                (children t')
        in
          (t', int, env')
        end
      | MaybeTy => continue ()
      | Decs => continue ()
      | Dec_Local => localize ()

      (* HERE LIES THE MAGIC *)
      | Dec_Val _ => ihatevaluedeclarations ()
      | Dec_Fun _ => ihatefunctiondeclarations ()
      (* And that's it: NO MORE MAGIC *)

      | Dec_Type => continue ()
      | Dec_Datatype => continue ()
      | Dec_Replication (dat1, dat2) =>
        datRep (unwrap dat1, unwrap dat2)
      | Dec_Abstype => continue ()
      | Dec_Exception => continue ()
      | Dec_Open ids =>
        let
          val (t', int, _) = continue ()
        in
          (t',
           int,
           List.foldl
             (fn (id, env') =>
                 env' ++ ValEnv.findStr env id
             )
             empty
             (map unwrap ids)
          )
        end
      | Dec_Fix _ => continue ()
      | Dec_Overload (p, var, ids) =>
        let
          val id = Variable.ident $ unwrap var
        (* we regard overloaded variables as new variables *)
          val (t', int, env') =
              newVal id
          val vid = ValEnv.findVal env' id
          val var = Wrap.modify (fn var => Variable.store var vid) var
          val node = Wrap.modify (const $ Dec_Overload (p, var, ids)) (this t')
        in
          (join node $ children t', int, env')
        end
      | Valbind_Plain => die "Valbind_Plain"
      | Valbind_Rec => die "Valbind_Rec"
      | Match => continue ()
      | Clause var =>
        (case children t of
           [pats, tyop, exp] =>
           let
             val (pats', int, env') = resolve (pats, int, env)
             (* val _ = *)
             (*     println *)
             (*       ((Ident.toString $ Variable.ident $ unwrap var) ^ ": " ^ *)
             (*        Layout.pretty NONE (ValEnv.show env')) *)
             val (tyop', int, _) = resolve (tyop, int, env)
             val (exp', int, _) = resolve (exp, int, env ++ env')

             val id = Variable.ident $ unwrap var
             val vid = ValEnv.findVal env id
             val var = Wrap.modify (fn var => Variable.store var vid) var

             val node = Wrap.modify (const $ Clause var) (this t)
           in
             (join
                (wrap
                   node
                   env
                   empty
                )
                [pats', tyop', exp'],
              int,
              empty)
           end
         | _ => fail "Illformed Clause"
        )
      | FlatClause => die "FlatClause"
      | Rule =>
        (case children t of
           [pat, exp] =>
           let
             val (pat', int, env') = resolve (pat, int, env)
             val (exp', int, _) = resolve (exp, int, env ++ env')
           in
             (join
                (wrap
                   (this t)
                   env
                   empty
                )
                [pat', exp'],
              int,
              empty)
           end
         | _ => fail "Illformed Rule"
        )
      | Datbinds => continue ()
      | Withtypes => continue ()
      | Tybind _ => continue ()
      | Exps => continue ()
      | Exp_Handle => continue ()
      | Exp_Orelse => continue ()
      | Exp_Andalso => continue ()
      | Exp_Typed => continue ()
      | Exp_App => continue ()
      | Exp_FlatApp => die "Exp_FlatApp"
      | Exp_Fn => continue ()
      | Exp_Case => continue ()
      | Exp_While => continue ()
      | Exp_If => continue ()
      | Exp_Raise => continue ()
      | Exp_Var var =>
        let
          val id = Variable.ident $ unwrap var
          val vid = ValEnv.findVal env id
              handle Domain =>
                     fail ("Undefined variable: " ^ Ident.toString id)
          val var = Wrap.modify (fn var => Variable.store var vid) var
        in
          newNode $ Exp_Var var
        end
        (* TODO: Store status and internal id at variable nodes *)
        (* continue () *)
      | Exp_SCon _ => continue ()
      | Exp_Selector _ => continue ()
      | Exp_Record => continue ()
      | Exp_Par => continue ()
      | Exp_Seq => continue ()
      | Exp_Tuple => continue ()
      | Exp_List => continue ()
      | Exp_Let => localize ()
      | Exp_LetSeq => localize ()
      | Label_Plain _ => continue ()
      | Label_Short var =>
        let
          val (t', int, env') = continue ()
          val id = Variable.ident $ unwrap var
          val env'' = ValEnv.newVal id
          val vid = ValEnv.findVal env'' id
          val var = Wrap.modify (fn var => Variable.store var vid) var
          val node =
              Wrap.modify (const $ Label_Short var) (this t')
        in
          (join node $ children t', int, env' ++ env'')
        end
      | MaybePat => continue ()
      | Pats => continue ()
      | Pat_Layered var =>
        let
          val (t', int, env') = continue ()
          val id = Variable.ident $ unwrap var
          val env'' = ValEnv.newVal id
          val vid = ValEnv.findVal env'' id
          val var = Wrap.modify (fn var => Variable.store var vid) var
          val node =
              Wrap.modify (const $ Pat_Layered var) (this t')
        in
          (join node $ children t', int, env' ++ env'')
        end
      | Pat_Typed => continue ()
      | Pat_App => continue ()
      | Pat_FlatApp => die "Pat_FlatApp"
      | Pat_Var var =>
        let
          val id = Variable.ident $ unwrap var
        in
          if ValEnv.isConOrExn env id then
            let
              val vid = ValEnv.findVal env id
              val var = Wrap.modify (fn var => Variable.store var vid) var
            in
              newNode $ Pat_Var var
            end
          else
            let
              val (t', int, env') = newVal id
              val vid = ValEnv.findVal env' id
              val var = Wrap.modify (fn var => Variable.store var vid) var
              val node =
                  Wrap.modify (const $ Pat_Var var) (this t')
            in
              (join node $ children t', int, env')
            end
        end
      | Pat_SCon _ => continue ()
      | Pat_Wild => continue ()
      | Pat_Tuple => continue ()
      | Pat_List => continue ()
      | Pat_Record => continue ()
      | Pat_FlexibleRecord => continue ()
      | Pat_Par => continue ()
      | Tys => continue ()
      | Ty_Tuple => continue ()
      | Ty_Record => continue ()
      | Ty_Var _ => continue ()
      | Ty_Con _ => continue ()
      | Ty_Par => continue ()
      | Ty_Arrow => continue ()
      | Unparsed => continue ()

    end
end
