structure Magic =
struct

fun fail s = raise Fail ("Magic: " ^ s)

fun rewriteMap (nill, cons, mapp) self ((ps1, e1) :: (ps2, e2) :: cs) =
    let open Grammar Tree
      val _ = println $ Variable.toString self
      fun isVar v =
          case Variable.load v of
            (_, _, ValEnv.Val) => true
          | _ => false
      fun isCons v = ValEnv.same (Variable.load v) (Variable.load cons)
      fun isNil v = ValEnv.same (Variable.load v) (Variable.load nill)
      fun bypass t = hd $ children t
      fun contexts ps =
          let
            fun xconsxs p =
                case (this p, children p) of
                  (Pat_App, [p1, p2]) =>
                  (case (this p1, this p2, children p2) of
                     (Pat_Var cons, Pat_Tuple, [p1, p2]) =>
                     (case (this p1, this p2) of
                        (Pat_Var x, Pat_Var xs) =>
                        if isCons cons andalso isVar x andalso isVar xs then
                          SOME (x, xs)
                        else
                          NONE
                      | _ => NONE)
                   | _ => NONE)
                | _ => NONE

            fun one p =
                case this p of
                  Pat_Par => one $ bypass p
                | Pat_Typed => one $ bypass p
                | _ =>
                  (case xconsxs p of
                     SOME (x, xs) => [(id, x, xs)]
                   | NONE => nil
                  ) @ (List.map
                         (fn (ins, x, xs) =>
                             (fn p' => join (this p) $ ins p',
                              x,
                              xs)
                         )
                         (many $ children p)
                      )
            and many ps =
                let
                  fun loop nil _ = nil
                    | loop (p :: ps) ps' =
                      let
                        val ctxs = one p
                      in
                        List.map
                          (fn (ins, x, xs) =>
                              (fn p' => rev ps' @ ins p' :: ps,
                               x,
                               xs)
                          )
                          (one p)
                        @ loop ps (p :: ps')
                      end
                in
                  loop ps nil
                end
          in
            many ps
          end
    in
      case (this e1, children e1) of
        (Exp_App, [e1, e2]) =>
        (case (this e1, this e2, children e2) of
           (Exp_Var v2, Exp_Tuple, [e1, e2]) =>
           if (* isNil v1 andalso *) isCons v2 then
             let open NormalForm
               val ctxs = contexts ps1
               val _ = println $ Show.int $ length ctxs
               fun braid [p] = kappa p
                 | braid (p1 :: p2 :: ps) = braid (join Pat_App [p1, p2] :: ps)
                 | braid _ = Crash.impossible "Magic.map.braid"
               fun pstoe ps = braid (singleton (Pat_Var self) :: ps)
               fun loop nil = NONE
                 | loop ((ins, x, xs) :: ctxs) =
                   let
               val _ = Layout.println NONE $ Grammar.showUnwrapped NONE e2
               val _ = Layout.println NONE $ Grammar.showUnwrapped NONE $
                                      pstoe $ ins $ singleton $ Pat_Var xs
               val _ = if equiv (e2, pstoe $ ins $ singleton $ Pat_Var xs) then println "foo" else println "bar"

in
                   if List.all
                        (fn v =>
                            not $ ValEnv.same (Variable.load v) (Variable.load xs)
                        ) $ FV [e1] andalso
                      equiv (e2, pstoe $ ins $ singleton $ Pat_Var xs)
                   then
                     SOME
                       (ins $ singleton $ Pat_Var xs,
                        join
                          Exp_App
                          [join
                             Exp_App
                             [singleton $ Exp_Var mapp,
                              join
                                Exp_Par
                                [join
                                   Exp_Fn
                                   [join
                                      Match
                                      [join
                                         Rule
                                         [singleton $ Pat_Var x,
                                          e1
                                         ]
                                      ]
                                   ]
                                ]
                             ],
                           singleton $ Exp_Var xs
                          ]
                       )
                   else
                     loop ctxs
end
             in
               Option.map (cs \> op::) $ loop ctxs
             end
           else
             NONE
         | _ => NONE)
      | _ => NONE
    end
  | rewriteMap _ _ _ = NONE

fun dust basis t =
    let open Grammar Tree NormalForm
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
      val mapp = var basis "map"

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

      fun loop t =
          let
            val ts = List.map (dust basis) $ children t
          in
            case (this t) of
              Match =>
              let
                val (n, cs) = extract ts
                val cs' = convert basis cs
                val cs' =
                    case n of
                      Clause v =>
                      (case rewriteMap (nill, cons, mapp) v cs' of
                         SOME cs => cs
                       | NONE => cs')
                    | _ => cs'
                val ts' = inject (n, cs')
              in
                join Match ts'
              end
            | n => join n ts
          end
    in
      loop t
    end
end
