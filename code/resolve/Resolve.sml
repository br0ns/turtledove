structure Resolve =
struct
exception Error of string
fun fail s = raise Error s

val eplus = Dictionary.plus
val eenv = Dictionary.empty
val renv = Infixing.resolve
(* val renv = id *)

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
            | Dec_Structure strbinds => continue ()
            | Dec_Signature sigbinds => continue ()
            | Dec_Functor fctbinds => continue ()
            | Prim => continue ()
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
