structure Resolve =
struct
exception Error of string

structure Map =
OrderedMapFn (
struct
type t = Path.t
fun compare x y = String.compare (Path.path x, Path.path y)
val toString = Path.toString
end
)

fun resolve (envfuns as {empty,
                         plus,
                         resolve,
                         rewrap}
            ) ast =
    let
      val eplus = plus
      val eenv = empty
      val bplus = Dictionary.plus
      val ebas = Dictionary.empty

      open MLBGrammar
      val this = Tree.this
      fun node t = Wrap.unwrap $ this t
      fun wrap n l r = identity $ Wrap.extend rewrap n l r
      val children = Tree.children
      val join = Tree.join
      fun left t = Wrap.left $ this t
      fun right t = Wrap.right $ this t

      fun fail s = raise Error s

      val mlbs = ref Map.empty
      fun mlblookup f = Map.lookup (!mlbs) f
      fun mlbinsert (x, y) = (mlbs := Map.update (!mlbs) (x, y) ; y)

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
                  val (ts, nbas, nenv) = loop' (children t, ebas, eenv)
                in
                  (join (wrap (this t) env nenv) ts, nbas, nenv)
                end

            fun localize _ =
                case children t of
                  [a, b] =>
                  let
                    val (a', nbas, nenv) = loop (a, bas, env)
                    val (b', nbas, nenv) =
                        loop (b, bplus bas nbas, eplus env nenv)
                  in
                    (join (wrap (this t) env nenv) [a', b'], nbas, nenv)
                  end
                | _ => fail
                         "Must have exactly two children in localization"
          in
            case node t of
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
              (case mlblookup file of
                 SOME x => x
               | NONE =>
                 let
                   val _ = Crash.debug ("Resolving " ^ Path.toString file)
                   val (ast', bas', env') = loop (ast, ebas, eenv)
                   val _ = Crash.debug "Done!"
                 in
                   mlbinsert
                     (file,
                      (join
                         (Wrap.wrap
                            (Dec_Include
                               {file     = file,
                                ast      = ast',
                                comments = comments
                               }
                            )
                            (rewrap (left t) env)
                            (rewrap (right t) env')
                         )

                         (* (modify *)
                         (*    (Dec_Include *)
                         (*       {file     = file, *)
                         (*        ast      = ast', *)
                         (*        comments = comments *)
                         (*       } *)
                         (*    ) *)
                         (*    {left = env, right = env'} *)
                         (* ) *)

                         nil,
                       ebas,
                       env'
                      )
                     )
                 end
              )
            | Dec_Source {file, ast, comments} =>
              (let
                 val (ast', env') = resolve (ast, env)
               in
                 (join
                    (Wrap.wrap
                       (Dec_Source {file = file, ast = ast', comments = comments})
                       (rewrap (left t) env)
                       (rewrap (right t) env')
                    )
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
      Dictionary.appi
        (fn (id, f) =>
            let
              fun iopt (SOME n) = Int.toString n
                | iopt NONE = ""
            in
              println
                (
                 (case f of
                    Fixity.InfixL n => "(L" ^ iopt n ^ ") "
                  | Fixity.InfixR n => "(R" ^ iopt n ^ ") "
                  | Fixity.Nonfix => ""
                  | Fixity.Op => "(op)"
                 ) ^ id
                )
            end
        ) env ;
      (* {ast = ast, env = env} *)
      ast
    end

val infixing = resolve {empty   = Infixing.empty,
                        plus    = Infixing.plus,
                        resolve = Infixing.resolve,
                        rewrap  = fn n => const n}
end

(* val f = Path.new "/home/morten/studie/turtledove/code/resolve/foo.sml" *)
(* val {ast, ...} = SMLParser.fromFile f *)
(* val (ast', _) = Infixing.resolve (ast, Dictionary.empty) *)

(* val _ = Layout.println NONE $ Grammar.show ast' *)
