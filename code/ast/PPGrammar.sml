structure PPGrammar =
struct

open Grammar


(* The unwrap function should either be the identity for unwrapped trees or
       Wrap.unwrap for wrapped trees *)
fun show unwrapnode unwrapid unwrapvar showt t =
    let
      open Layout
      infix ^^ ++ \ & \\ &&

      val node = (unwrapnode o Tree.this)
      val showid = (Ident.show o unwrapid)
      val showvar = (Variable.show o unwrapvar)

      (* For easy recursive call *)
      val show' = show unwrapnode unwrapid unwrapvar showt

      fun showSCon scon =
          case scon of
            SCon.String s => "\"" ^ s ^ "\""
          | SCon.Char c => "#\"" ^ c ^ "\""
          | SCon.Int i => i
          | SCon.Real r => r
          | SCon.Word w => w

      (* Helper function for indenting matches correctly *)
      fun PPMatch numFstIndent numIndent sep (m::ms) =
          List.foldl (fn (a,b) => sep (b, (indent numIndent $ txt "|" ++ (show' a))))
                     (indent numFstIndent $ show' m)
                     ms
        | PPMatch _ _ _ [] = die "Empty match"

      (* Helper function to call PPMatch after getting the children of the tree *)
      fun PPMatch' numFstIndent numIndent sep match =
          case Tree.children match of
            ms => PPMatch numFstIndent numIndent sep ms

      fun die s = Crash.impossible $ "PPGrammar: " ^ s
    in
      case node t of
        Topdecs => (* Topdec list *)
        (
         case Tree.children t of
           [] => die "Empty Topdec list in Topdecs"
         | topdecs => vsep $ List.map show' topdecs
        )

      (* Decs *)
      | Decs => (* Dec list *)
        (
         case Tree.children t of
           [] => die "Empty Decs"
         | decs => align $ vsep $ List.map show' decs
        )

      (* Dec *)
      | Dec_Local => (* [Decs, Decs] *)
        (
         case Tree.children t of
           [decLocal, decIn] => txt "local" \
                                    (indent 2 $ show' decLocal) \
                                    txt "in" \
                                    (indent 2 $ show' decIn) \
                                    txt "end"
         | _ => die "Empty or malformed Dec_Local"
        )

      | Dec_Val tyvars => (* Valbind list *)
        (
         (* TODO: Multiple recursive definitions might not show correct *)
         (case Tree.children t of
            [] => die "Empty Valbind list in Dec_Val"
          | valb::valbs => 
            (* Fist valbind is with "val", rest with "and" *)
            List.foldl (fn (a,b) => b \ (txt "and" ++ show' a))
                       (txt "val" ^^          
                        ((* TODO: The tyvars might not be printed correct. *)
                         case tyvars of
                           [] => txt ""
                         | _ => hsep $ punctuate comma $ List.map showid tyvars
                        ) ++
                       show' valb
                       )
                       valbs
         )
        )
        
      | Dec_Fun tyvars => (* Match list (Clause) *)
        (align $
               txt "fun" ++
               (* TODO: The tyvars might not be printed correct. *)
               (
                case tyvars of
                  [] => txt ""
                | _ => hsep $ punctuate comma $ List.map showid tyvars
               ) ^^
               (case Tree.children t of
                  [] => die "Empty Match list in Dec_Fun"
                | [match] => show' match
                | matchs => die "Mutually recursive function definitions are not supported in Dec_Fun"

               )
        )

      | Dec_Type => (* Tybind list *)
        (
         txt "type" ++
             (
              case Tree.children t of
                [] => die "Emtpy Tybind list in Dec_Type"
              | tybinds => vsep $ punctuate (txt "and") $ List.map show' tybinds
             )
        )
      (*
      | Dec_Datatype => (* [Datatypes, Withtypes] *)
        (
         case Tree.children t of
        )

      | Dec_Replication of 'ident * 'ident => (* [] *)
                           (
                            case Tree.children t of
                           )
                         | Dec_Abstype => (* [Datbinds, Withtypes, Decs] *)
                           (
                            case Tree.children t of
                           )
                         | Dec_Exception => (* (Constructor | Replication) list *)
                           (
                            case Tree.children t of
                           )
       *)
      | Dec_Open ids =>
        (
         txt "open" ++
             (
              case ids of
                [] => die "Empty Dec_Open"
              | _ => hsep $ List.map showid ids
             )
        )
      (*
      | Dec_Fix of Fixity.t * 'ident list =>
                   (
                    case Tree.children t of
                   )
                 | Dec_Overload of int option * 'var * 'ident list =>  (* [Ty] *)
                                   (
                                    case Tree.children t of
                                   )
       *)

      (* Valbind *)
      | Valbind_Plain => (* [Pat, Exp] *)
        (
         case Tree.children t of
           [pat, exp] => show' pat ++
                               eq ++
                               show' exp
         | _ => die "Empty or malformed Valbind_Plain"
        )
      | Valbind_Rec => (* [Pat, Match] *)
        (
         case Tree.children t of
           [pat, exp] => txt "rec" ++
                             show' pat ++
                             eq ++
                             show' exp
         | _ => die "Empty or malformed Valbind_Rec"
        )

      (* Match *)
      | Match => (* (Clause | Rule) list *)
        (
         PPMatch 0 2 op\ (Tree.children t)
        )

      (* Clause *)
      | Clause funname =>(* [Pats, MaybeTy, Exp] *)
        (
         showvar funname ++
                 (
                  case Tree.children t of
                    [pats, maybety, exp] =>
                    (
                     case Tree.children maybety of
                       [] => show' pats ++
                                   txt "=" ++
                                   show' exp
                     | _ => die "Clause has a maybeTy, this is not implemented!!!"
                    )
                  | _ => die "Empty or malformed Clause"
                 )
        )

      (* Only present at the parsing, is transformed to a clause after infixing
and other passes of the ast *)
      (*
      | FlatClause => (* [Pats, MaybeTy, Exp] *)
        (
        )
       *)
      | Rule => (* [Pat, Exp] *)
        (
         case Tree.children t of
           [pat, exp] => show' pat ++
                               txt "=>" ++
                               show' exp
         | _ => die "Empty or malformed Rule"
        )
      (* Exps *)
      | Exps =>  (* Exp list *)
        (
         vsep $ List.map show' (Tree.children t)
        )
      (* Exp *)
      | Exp_Handle => (* [Exp, Match] *)
        (
         case Tree.children t of
           [exp, match] => (show' exp)
                             ++ align $ txt "handle"
                             ++ (PPMatch' 0 2 op\  match)
         | _ => die $ "Empty or malformed Exp_Handle: "
        )
      | Exp_Raise =>  (* [Exp] *)
        (
         case Tree.children t of
           [exp] => txt "raise" ++ show' exp
         | _ => die "Empty or malformed Exp_Raise"
        )
      | Exp_If => (* [Exp, Exp, Exp] *)
        (
         case Tree.children t of
           [expCond, expThen, expElse] =>
           align (
           txt "if" ++ show' expCond ++ txt "then" \
               (indent 2 $ show' expThen) \
               txt "else" \
               (indent 2 $ show' expElse)
           )
         | _ => die "Empty or malformed Exp_If"
        )
      | Exp_Orelse => (* [Exp, Exp] *)
        (
         case Tree.children t of
           [exp1, exp2] =>  show' exp1 ++ txt "orelse" ++ show' exp2
         | _ => die "Empty or malformed Exp_Orelse"
        )
      | Exp_Andalso =>  (* [Exp, Exp] *)
        (
         case Tree.children t of
           [exp1, exp2] =>  show' exp1 ++ txt "andalso" ++ show' exp2
         | _ => die "Empty or malformed Exp_Andalso"
        )
      | Exp_Unit => txt "()"
      | Exp_App => (* [Exp, Exp] *)
        (
         case Tree.children t of
           [exp1, exp2] =>
           (case (node exp1, node exp2) of
              (Exp_Var v, Exp_Tuple) =>
              if Ident.isInfix $ Variable.ident $ unwrapvar v then
                case Tree.children exp2 of
                  [expl, expr] =>
                  show' expl ++ show' exp1 ++ show' expr
                | _ => die "Needs two operands in infix expression of Exp_App"
              else
                show' exp1 ++ show' exp2
            | _ => show' exp1 ++ show' exp2
           )
         | _ => die "Empty or malformed Exp_App"
        )
      | Exp_FlatApp => (* Exp list *)
        (
         case Tree.children t of
           [] => die "Empty Exp_FlatApp"
         | exps => align $ hsep $ List.map show' exps
        )
      | Exp_Fn => (* [Match] *)
        (
         case Tree.children t of
           [match] => align (txt "fn" ++ PPMatch' 0 1 op\ match)
         | _ => die "Empty or malformed list of matches in Exp_Fn"
        )
      | Exp_Case => (* [Exp, Match] *)
        (
         case Tree.children t of
           [exp, match] =>
           align (txt "case" ++ show' exp ++ txt "of" \
                      PPMatch' 2 0 op\ match
                 )
         | _ => die "Empty or malformed Exp_Case"
        )
      | Exp_SCon scon => txt $ showSCon scon
      | Exp_While => (* [Exp, Exp] *)
        (
         case Tree.children t of
           [expWhile, expDo] =>
           align (
           txt "while" \
               (indent 2 $ show' expWhile) \
               txt "do" \
               (indent 2 $ show' expDo)
           )
         | _ => die "Empty or malformed Exp_While"
        )
      | Exp_Let => (* [Decs, Exp] *)
        (
         case Tree.children t of
           [decs, exp] => align $ txt "let" \
                                (indent 2 $ show' decs) \
                                txt "in" \
                                (indent 2 $ show' exp)
         | _ => die "Empty or malformed Exp_Let"
        )
      | Exp_LetSeq => (* [Decs, Exps] *)
        (
         case Tree.children t of
           [decs, exps] =>
           (
            case Tree.children exps of
              [] => die "Empty exp_seq in Exp_LetSeq"
            | exp_seq => align $ txt "let" \
                               (indent 2 $ show' decs) \
                               txt "in" \
                               (indent 2 $
                                       vsep $ punctuate semi $
                                       List.map show' exp_seq
                               )
           )
         | _ => die "Empty or malformed Exp_Let"
        )
      | Exp_Par => (* [Exp] *)
        (
         case Tree.children t of
           [exp] => align $ parens $ show' exp
         | _ => die "Empty or malformed Exp_Par"
        )
      | Exp_Tuple => (* Exp list *)
        (
         case Tree.children t of
           exps => align $ parens $ hsep $ punctuate comma $ List.map show' exps
        )
      | Exp_List => (* Exp list *)
        (
         case Tree.children t of
           exps => align $ brackets $ hsep $ punctuate comma $ List.map show' exps

        )
      | Exp_Record => (* Label list *)
        (
         case Tree.children t of
           exps => align $ braces $ hsep $ punctuate comma $ List.map show' exps
        )
      | Exp_Seq => (* Exp list *)
        (
         case Tree.children t of
           exp_seq => align $ parens $ vsep $ punctuate semi $ List.map show' exp_seq
        )
      | Exp_Var var => showvar var
      | Exp_Selector ident => showid ident

      | Exp_Typed => die "you are ugly" (* [Exp, Ty] *)

      (* MaybePat *)
      | MaybePat => (* [Pat] | [] *)
        (
         case Tree.children t of
           [pat] => show' pat
         | [] => txt ""
         | _ => die "Malformed MaybePat"
        )

      (* Pats *)
      | Pats => (* Pat list *)
        (
         case Tree.children t of
           [] => die "Empty Pat list in Pats"
         | pats => vsep $ List.map show' pats
        )

      (* Pat *)
      | Pat_Layered var => (* [pat] *)
        (
         (showvar var) ++
                       txt "as" ++
                       (
                        case Tree.children t of
                          [pat] => show' pat
                        | _ => die "Empty or malformed Pat_layered"
                       )
        )
      | Pat_Typed => (* [pat, ty] *)
        (
         case Tree.children t of
           [pat, ty] => show' pat ++
                              colon ++
                              show' ty
         | _ => die "Empty or malformed Pat_Typed"
        )
      | Pat_App =>  (* [pat, pat] *)
        (
         case Tree.children t of
           [pat1, pat2] =>
           (case (node pat1, node pat2) of
              (Pat_Var v, Pat_Tuple) =>
              if Ident.isInfix $ Variable.ident $ unwrapvar v then
                case Tree.children pat2 of
                  [patl, patr] =>
                  show' patl ++ show' pat1 ++ show' patr
                | _ => die "Needs two operands in infix pattern of Pat_App"
              else
                show' pat1 ++ show' pat2
            | _ => show' pat1 ++ show' pat2
           )
         | _ => die "Empty or malformed Pat_App"
        )
      (*
      (* Not used after infixing and other stuff *)
      | Pat_FlatApp => (* pat list *)
        (
         case Tree.children t of
        )
       *)
      | Pat_Var var =>
        (
         showvar var
        )
      | Pat_SCon scon =>
        (
         txt $ showSCon scon
        )
      | Pat_Wild =>
        (
         txt "_"
        )
      | Pat_Tuple =>
        (
         case Tree.children t of
           [] => die "Empty Pat_Tuple"
         | pats => parens $ hsep $ punctuate comma $ List.map show' pats
        )
      | Pat_List =>
        (
         case Tree.children t of
           pats => brackets $ hsep $ punctuate comma $ List.map show' pats
        )
      (*
      | Pat_Record =>
        (
         case Tree.children t of
        )
      | Pat_FlexibleRecord =>
        (
         case Tree.children t of
        )
       *)

      | Pat_Par => (* [Pat] *)
        (
         case Tree.children t of
           [pat] => parens $ show' pat
         | _ => die "Empty or malformed Pat_Par"
        )


      | _ => die $ "pritty printer not implemented for this token: " ^

             (Layout.pretty NONE $ showt (SOME 1) t)
    end


fun showWrapped t = show Wrap.unwrap Wrap.unwrap Wrap.unwrap Grammar.showWrapped t
fun showUnwrapped t = show id id id Grammar.showUnwrapped t
end
