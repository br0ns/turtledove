structure Main =
struct

exception Error of Layout.t
fun fail s = raise Error s

fun init path =
    let
      open MLBGrammar
      val this = Tree.this
      val children = Tree.children
      fun node t = Wrap.unwrap $ this t
      val children = Tree.children
      val join = Tree.join
      fun wrap n l r = identity $ Wrap.extend (const id) n l r

      fun loop (t, files, dep, bas) =
          let
            fun baslookup id =
                case Dictionary.lookup bas id of
                  SOME dep => dep
                | NONE     => fail $ Layout.txt ("Undefined variable: " ^ id)

            fun continue _ =
                let
                  (* Funny fact: It took me a whole day to find this bug:
                   * fun loop' (x as (nil, _, _, _)) = x
                   * Can you figure out what is wrong?
                   *)
                  fun loop' (nil, files, dep', bas') = (nil, files, dep', bas')
                    | loop' (t :: ts, files, dep', bas') =
                      let
                        val (t', files, dep'', bas'') =
                            loop (t,
                                  files,
                                  Path.Set.union dep dep',
                                  Dictionary.plus bas bas'
                                 )
                        val (ts', files, dep'', bas'') =
                            loop' (ts,
                                   files,
                                   Path.Set.union dep' dep'',
                                   Dictionary.plus bas' bas''
                                  )
                      in
                        (t' :: ts', files, dep'', bas'')
                      end
                  val (ts, files, dep', bas') =
                      loop' (children t, files, Path.Set.empty, Dictionary.empty)
                in
                  (join
                     (wrap
                        (this t)
                        dep
                        dep'
                     ) ts,
                   files,
                   dep',
                   bas')
                end
          in
            case node t of
              Dec_Source file =>
              (case Path.Map.lookup files file of
                 SOME (x, dep') => (x, files, dep', Dictionary.empty)
               | NONE =>
                 let
                   val {ast, comments} = SMLParser.fromFile file
                       handle
                       SMLParser.LexError (p, e) => fail $ Layout.txt "Lex"
                     | SMLParser.YaccError (p, e) => fail $ Layout.txt "Yacc"

                   val dep' = Path.Set.singleton file
                   val t' =
                       join
                         (Wrap.wrap
                            (Dec_Source
                               {file = file, ast = ast, comments = comments}
                            )
                            dep
                            dep'
                         )
                         nil
                   val files = Path.Map.update files (file, (t', dep'))
                 in
                   (t',
                    files,
                    dep',
                    Dictionary.empty)
                 end)
            | Dec_Include {file, ast, comments} =>
              (case Path.Map.lookup files file of
                 SOME (t', dep') => (t', files, dep', Dictionary.empty)
               | NONE =>
                 let
                   val (ast', files, dep', _) =
                       loop (ast, files, Path.Set.empty, Dictionary.empty)
                   val t' =
                       join
                         (Wrap.wrap
                            (Dec_Include
                               {file = file, ast = ast', comments = comments}
                            )
                            dep
                            dep'
                         )
                         nil
                   val files = Path.Map.update files (file, (t', dep'))
                 in
                   (t',
                    files,
                    dep',
                    Dictionary.empty)
                 end)
            | Basbind id =>
              let
                val (t', files, dep', bas') = continue ()
              in
                (t', files, dep', Dictionary.update bas' (id, dep'))
              end
            | Exp_Var id =>
              let
                val (t', files, _, bas') = continue ()
              in
                (t', files, baslookup id, bas')
              end
            | Dec_Open ids =>
              let
                val (t', files, _, bas') = continue ()
              in
                (t',
                 files,
                 List.foldl
                   (fn (id, dep) =>
                       Path.Set.union dep $ baslookup id
                   )
                   Path.Set.empty
                   ids,
                 bas')
              end
            | _ => continue ()

          end

      val {ast, comments} = MLBParser.fromFile path
      val (t, files, dep, bas) =
          loop (ast, Path.Map.empty, Path.Set.empty, Dictionary.empty)
    in
      {file = path, ast = t, comments = comments}
    end
    (* handle IO.Io {name, cause = OS.SysErr (err, se), ...} => *)
         (* | IO.Io {name, ...} => *)

end
