structure Resolve =
struct
exception Error of string

fun resolve (envfuns as {empty, plus, resolve}) ast =
    let
      val eplus = plus
      val eenv = empty
      val bplus = Dictionary.plus
      val ebas = Dictionary.empty

      open MLBGrammar
      val this = Tree.this
      val children = Tree.children
      val join = Tree.join
      val node = identity o this

      fun fail s = raise Error s

      fun loop (t, bas, env) =
          let
            fun lookup id =
                case Dictionary.lookup bas id of
                  SOME env => env
                | NONE     => fail ("Undefined variable: " ^ id)

            fun openbas id (t, bas, env) =
                (t, bas, eplus env $ lookup id)

            fun continue _ =
                let
                  fun loop' (nil, nbas, nenv) = (nil, nbas, nenv)
                    | loop' (t :: ts, nbas, nenv) =
                      let
                        val (t', nbas', nenv') =
                            loop (t, bplus bas nbas, eplus env nenv)
                        val (ts', nbas', nenv') =
                            loop' (ts, bplus nbas nbas', eplus nenv nenv')
                      in
                        (t' :: ts', nbas', nenv')
                      end
                  val (ts, bas, env) = loop' (children t, ebas, eenv)
                in
                  (join (node t) ts, bas, env)
                end

            fun localize _ =
                case children t of
                  [a, b] =>
                  let
                    val (a', nbas, nenv) = loop (a, bas, env)
                    val (b', nbas, nenv) =
                        loop (b, bplus bas nbas, eplus env nenv)
                  in
                    (join (node t) [a', b'], nbas, nenv)
                  end
                | _ => fail
                         "Must have exactly two children in localization"
          in
            case this t of
              Basdecs => continue ()
            | Basbind id =>
              let
                val (t', bas', env') = continue ()
              in
                (t', Dictionary.update bas (id, env'), empty)
              end
            | Exp_Basis => continue ()
            | Exp_Let => localize ()
            | Exp_Var id => openbas id $ continue ()
            | Dec_Basis => continue ()
            | Dec_Local => localize ()
            | Dec_Include {file, ast, comments} =>
              let
                val (ast', bas', env') = loop (ast, ebas, eenv)
              in
                (join
                   (Dec_Include
                      {file     = file,
                       ast      = ast',
                       comments = comments
                      }
                   )
                   nil,
                 ebas,
                 env'
                )
              end
            | Dec_Source {file, ast, comments} =>
              (let
                val (ast', env') = resolve (ast, env)
              in
                (join
                   (Dec_Source {file = file, ast = ast', comments = comments})
                   nil,
                 ebas,
                 env'
                )
              end handle _ => fail $ Path.toString file)
            | Dec_Open ids =>
              List.foldl
                (fn (id, a) => openbas id a)
                (continue ())
                ids
            | Dec_Ann anns => continue ()
            | Dec_Structure strbinds => continue ()
            | Dec_Signature sigbinds => continue ()
            | Dec_Functor fctbinds => continue ()
            | Prim => continue ()
          end
      val (ast, bas, env) = loop (ast, ebas, eenv)
    in
      (* {ast = ast, env = env} *)
      ast
    end

val infixing = resolve {empty   = Infixing.empty,
                        plus    = Infixing.plus,
                        resolve = Infixing.resolve}
end

(* val f = Path.new "/home/morten/studie/turtledove/code/resolve/foo.sml" *)
(* val {ast, ...} = SMLParser.fromFile f *)
(* val (ast', _) = Infixing.resolve (ast, Dictionary.empty) *)

(* val _ = Layout.println NONE $ Grammar.show ast' *)
