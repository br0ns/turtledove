fun test () = 
    let
      val args = CommandLine.arguments()
    in
      if length args = 1 then  
        let
          val {rules, comments} = RuleParser.fromFile $ Path.new (hd args)
        in
         Layout.println NONE $ RuleGrammar.show rules
        end
      else
        print "Must be called with 1 argument, namely a path of a rule file to parse\n"
    end handle RuleParser.Parse r => Layout.println NONE r


val _ = test ()
