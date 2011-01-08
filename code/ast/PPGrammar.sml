local
  open Grammar
in

fun PPGrammar showid showvar t =
    let
      open Layout
      infix ^^ ++ \ & \\ &&

      val PPGrammar' = PPGrammar showid showvar

      fun node t = Wrap.unwrap $ Tree.this t

      fun showSCon scon =
          case scon of
            SCon.String s => "\"" ^ s ^ "\""
          | SCon.Char c => "#\"" ^ c ^ "\""
          | SCon.Int i => i
          | SCon.Real r => r
          | SCon.Word w => w

      fun PPMatch numFstIndent sep (m::ms) = 
          (* Ugly hack with -1 to make the first line "indent" compared to the *)
          (* rest of the lines*)
          hang (numFstIndent * ~1) $ 
               List.foldl (fn (a,b) => sep (b, (txt "|" \  (PPGrammar' a))))
                          (PPGrammar' m) 
                          ms
        | PPMatch _ _ [] = die "Empty match"

      fun PPMatch' numFstIndent sep match = 
          case Tree.children t of
            ms => PPMatch numFstIndent sep ms

      fun die s = Crash.impossible $ "PPGrammar: " ^ s
    in
      case node t of
       (* Exps *)        
        Exps => vsep $ List.map PPGrammar' (Tree.children t) (* Exp list *)
        
       (* Exp *)
       | Exp_Handle => (* [Exp, Match] *)
         (
          case Tree.children t of
            [exp, match] => (PPGrammar' exp) 
                                ++ align $ txt "handle"
                                ++ (PPMatch' 2 op\  match)
          | _ => die $ "Empty or malformed Exp_Handle: "
         ) 
       | Exp_Raise =>  (* [Exp] *)  
         (
          case Tree.children t of
            [exp] => txt "raise" ++ PPGrammar' exp
          | _ => die "Empty or malformed Exp_Raise"
         )
       | Exp_If => (* [Exp, Exp, Exp] *)
         (
          case Tree.children t of
            [expCond, expThen, expElse] => 
            align $ 
                  txt "if" ++ PPGrammar' expCond ++ txt "then" \
                  (indent 2 $ PPGrammar' expThen) \
                  txt "else" \
                  (indent 2 $ PPGrammar' expElse)
          | _ => die "Empty or malformed Exp_If"
         )
       | Exp_Orelse => (* [Exp, Exp] *)
         (
          case Tree.children t of
            [exp1, exp2] =>  PPGrammar' exp1 ++ txt "orelse" ++ PPGrammar' exp2
          | _ => die "Empty or malformed Exp_Orelse"
         )
       | Exp_Andalso =>  (* [Exp, Exp] *)
         (
          case Tree.children t of
            [exp1, exp2] =>  PPGrammar' exp1 ++ txt "andalso" ++ PPGrammar' exp2
          | _ => die "Empty or malformed Exp_Andalso"
         )
       | Exp_Unit => txt "()"
       | Exp_App => (* [Exp, Exp] *)
         (
          case Tree.children t of
            [exp1, exp2] => PPGrammar' exp1 ++ PPGrammar' exp2
          | _ => die "Empty or malformed Exp_App"
         )
       | Exp_FlatApp => (* Exp list *)
         (
          case Tree.children t of
            [] => die "Empty Exp_FlatApp"
          | exps => align $ hsep $ List.map PPGrammar' exps
         )
       | Exp_Fn => (* [Match] *)
         (
          case Tree.children t of
            [] => die "Empty list of matches in Exp_Fn"
          | ms => align $ txt "fn" ++ PPMatch 2 op\ ms
         )
       | Exp_Case => (* [Exp, Match] *)
         (
          case Tree.children t of
            [exp, match] => align $ 
                                  txt "case" ++ PPGrammar' exp ++ txt "of" \
                                  PPMatch' 2 op\ match
          | _ => die "Empty or malformed Exp_Case"
         )
       | Exp_SCon scon => txt $ showSCon scon
       | Exp_While => (* [Exp, Exp] *)
         (
          case Tree.children t of
            [expWhile, expDo] => align $ txt "while" \ 
                                       (indent 2 $ PPGrammar' expWhile) \
                                       txt "do" \
                                       (indent 2 $ PPGrammar' expDo)
          | _ => die "Empty or malformed Exp_While"
         )
       | Exp_Let => (* [Decs, Exp] *)
         (
          case Tree.children t of
            [decs, exp] => align $ txt "let" \
                                 (indent 2 $ PPGrammar' decs) \
                                 txt "in" \ 
                                 (indent 2 $ PPGrammar' exp)
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
                                (indent 2 $ PPGrammar' decs) \
                                txt "in" \ 
                                (indent 2 $ 
                                        vsep $ punctuate semi $ 
                                        List.map PPGrammar' exp_seq
                                )         
            )                
          | _ => die "Empty or malformed Exp_Let"
         )
       | Exp_Par => (* [Exp] *)
         (
          case Tree.children t of
            [exp] => align $ parens $ PPGrammar' exp
          | _ => die "Empty or malformed Exp_Par"
         )
       | Exp_Tuple => (* Exp list *)
         (
          case Tree.children t of
            exps => align $ parens $ hsep $ punctuate comma $ List.map PPGrammar' exps
         )
       | Exp_List => (* Exp list *)
         (
          case Tree.children t of
            exps => align $ brackets $ hsep $ punctuate comma $ List.map PPGrammar' exps
         )
       | Exp_Record => (* Label list *)
         (
          case Tree.children t of
            exps => align $ braces $ hsep $ punctuate comma $ List.map PPGrammar' exps
         )         
       | Exp_Seq => (* Exp list *)
         (
          case Tree.children t of
            exp_seq => align $ parens $ vsep $ punctuate semi $ List.map PPGrammar' exp_seq
         )
       | Exp_Var var => txt $ showvar var
       | Exp_Selector ident => txt $ showid ident

       | Exp_Typed => die "you are ugly" (* [Exp, Ty] *)


       | _ => die "pritty printer not implemented for this token"
    end
end
