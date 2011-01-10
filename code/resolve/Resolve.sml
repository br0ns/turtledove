structure Resolve =
struct
exception Error of string
fun fail s = raise Fail s

type basis = {infixing    : Infixing.basis,
              environment : ValEnv.t,
              interface   : IntEnv.t}
fun eplus {infixing = inf1, environment = env1, interface = int1}
          {infixing = inf2, environment = env2, interface = int2} =
    {infixing    = Infixing.plus inf1 inf2,
     environment = ValEnv.++ (env1, env2),
     interface   = IntEnv.++ (int1, int2)
    }
val eenv =
    {infixing    = Infixing.empty,
     environment = ValEnv.empty,
     interface   = IntEnv.empty
    }
val primenv =
    {infixing    = Infixing.empty,
     environment = ValEnv.prim,
     interface   = IntEnv.empty
    }

fun renv (t, {infixing = inf, environment = env, interface = int}) =
    let
      val (t', inf') = Infixing.resolve (t, inf)
      val t' = Grammar.transform Variable.ofIdent t'
      val (t'', int', env') = Environment.resolve (t', int, env)
    in
      (t'',
       {infixing    = inf',
        environment = env',
        interface   = int'
      })
    end

fun init ast =
    let
      val bplus = Dictionary.plus
      val ebas = Dictionary.empty

      open MLBGrammar
      val this = Tree.this
      fun node t = Wrap.unwrap $ this t
      fun join n ts = Tree.join (identity n) ts
      val children = Tree.children

      fun loop (t, mlbs, bas, env) =
          let
            fun lookup id =
                case Dictionary.lookup bas id of
                  SOME env => env
                | NONE     => fail ("Undefined variable: " ^ id)

            fun openbas id (t, mlbs, bas, env) =
                (t, mlbs, bas, lookup id)

            fun continue _ =
                let
                  fun loop' (nil, mlbs, bas', env') = (nil, mlbs, bas', env')
                    | loop' (t :: ts, mlbs, bas', env') =
                      let
                        val (t', mlbs, bas'', env'') =
                            loop (t, mlbs, bplus bas bas', eplus env env')
                        val (ts', mlbs, bas'', env'') =
                            loop' (ts, mlbs, bplus bas' bas'', eplus env' env'')
                      in
                        (t' :: ts', mlbs, bas'', env'')
                      end
                  val (ts, mlbs, bas', env') = loop' (children t, mlbs, ebas, eenv)
                in
                  (join (this t) ts, mlbs, bas', env')
                end

            fun localize _ =
                case children t of
                  [a, b] =>
                  let
                    val (a', mlbs, bas', env') = loop (a, mlbs, bas, env)
                    val (b', mlbs, bas', env') =
                        loop (b, mlbs, bplus bas bas', eplus env env')
                  in
                    (join (this t) [a', b'], mlbs, bas', env')
                  end
                | _ => fail
                         "Must have exactly two children in localization"
          in
            case node t of
              Basdecs => continue ()
            | Basbind id =>
              let
                val (t', mlbs, bas', env') = continue ()
              in
                (t', mlbs, Dictionary.update bas (id, env'), eenv)
              end
            | Exp_Basis => continue ()
            | Exp_Let => localize ()
            | Exp_Var id => openbas id $ continue ()
            | Dec_Basis => continue ()
            | Dec_Local => localize ()
            | Dec_Include {file, ast, comments, basis} =>
              (case Path.Map.lookup mlbs file of
                 SOME (t, env') => (t, mlbs, ebas, env')
               | NONE =>
                 let
                   val (ast', mlbs, bas', env') = loop (ast, mlbs, ebas, eenv)
                   val t = Tree.join
                             (Wrap.wrap
                                (Dec_Include {file     = file,
                                              ast      = ast',
                                              comments = comments,
                                              basis    = env'}
                                )
                                (Wrap.left $ this t)
                                (Wrap.right $ this t)
                             )
                             nil
                   val mlbs = Path.Map.update mlbs (file, (t, env'))
                 in
                   (t, mlbs, ebas, env')
                 end
              )
            | Dec_Source {file, ast, comments} =>
              let
                val (ast', env') = renv (ast, env)
                    handle Environment.Error (p, e) =>
                           let
                             open Layout infix && \
                             val st = SourceText.fromFile file
                             val p = SourceText.showPos st p
                             val e = p && colon \ indent 2 (txt e)
                           in
                             fail $ Layout.pretty NONE e
                           end
                         | Fail e => fail (Path.toString file ^ ": " ^ e)
                         | _ => fail (Path.toString file)
              in
                (Tree.join
                   (Wrap.wrap
                      (Dec_Source {file = file, ast = ast', comments = comments})
                      (Wrap.left $ this t)
                      (Wrap.right $ this t)
                   )
                   nil,
                 mlbs,
                 ebas,
                 env'
                )
              end
            | Dec_Open ids =>
              List.foldl
                (fn (id, a) => openbas id a)
                (continue ())
                ids
            | Dec_Ann anns => continue ()
            (* TODO: fix rebinding of structures, signatures and functors *)
            | Dec_Structure strbinds =>
              let
                val {infixing    = inf,
                     environment = venv,
                     interface   = int} =
                    env

                val venv' =
                    List.foldl
                      (fn (binding, env) =>
                          ValEnv.++ (env, ValEnv.strRep venv binding)
                      )
                      ValEnv.empty
                      strbinds
              in
                (join
                   (this t)
                   nil,
                 mlbs,
                 ebas,
                 {infixing    = inf,
                  environment = venv',
                  interface   = int}
                )
              end
            | Dec_Signature sigbinds =>
              let
                val {infixing    = inf,
                     environment = venv,
                     interface   = int} =
                    env

                val int' =
                    List.foldl
                      (fn (binding, int') =>
                          IntEnv.++ (int', IntEnv.sigRep int binding)
                      )
                      IntEnv.empty
                      sigbinds
              in
                (join
                   (this t)
                   nil,
                 mlbs,
                 ebas,
                 {infixing    = inf,
                  environment = venv,
                  interface   = int'}
                )
              end
            | Dec_Functor fctbinds =>
              let
                val {infixing    = inf,
                     environment = venv,
                     interface   = int} =
                    env

                val int' =
                    List.foldl
                      (fn (binding, int') =>
                          IntEnv.++ (int', IntEnv.funRep int binding)
                      )
                      IntEnv.empty
                      fctbinds
              in
                (join
                   (this t)
                   nil,
                 mlbs,
                 ebas,
                 {infixing    = inf,
                  environment = venv,
                  interface   = int'}
                )
              end
            | Prim => (join (this t) nil, mlbs, ebas, primenv)
          end
      val (ast, mlbs, bas, env) = loop (ast, Path.Map.empty, ebas, eenv)
    in
      ast
    end


(* val dependencies = *)
(*     resolve *)
(*     {empty = Set.empty, *)
(*      plus = Set.union, *)
(*      resolve = fn (env, path) => (Set.insert env path, path), *)
(*      rewrap = const id *)
(*     } *)

(* val infixing = resolve {empty   = Infixing.empty, *)
(*                         plus    = Infixing.plus, *)
(*                         resolve = *)
(*                      fn ({file, ast, comments}, bas) => *)
(*                         let *)
(*  Infixing.resolve, *)
(*                         rewrap  = fn n => const n} *)
end

(* val f = Path.new "/home/morten/studie/turtledove/code/resolve/foo.sml" *)
(* val {ast, ...} = SMLParser.fromFile f *)
(* val (ast', _) = Infixing.resolve (ast, Dictionary.empty) *)

(* val _ = Layout.println NONE $ Grammar.show ast' *)
