structure MLBGrammar =
struct
    type basid = string
    type basids = basid list
    type file = string (* File.t *)
    
    datatype basexp = Bas of basdecs
                    | Let of basdecs * basexp
                    | Var of basid

         (* Basis is a list of bindings because of the 'and' keyword as in
            basis foo = bas foo.sml end
              and bar = bas bar.sml end
          *)
         and basdec = Basis of basbinds
                    | Local of basdecs * basdecs
                    | Include of file
                    | Source of file
                    | Open of basids
    withtype basdecs = basdec list
         and basbinds = (basid * basexp) list

    (* structure Show : sig *)
    (*     val basexp : basexp -> Report.t *)
    (*     val basdec : basdec -> Report.t *)
    (* end = *)

    local
        open Report
        infix ++
    in
    fun showBasexp (Bas basdecs) =
        text "bas" ++
        indent (showBasdecs basdecs) ++
        text "end"
      | showBasexp (Let (basdecs, basexp)) =
        text "let" ++
        indent (showBasdecs basdecs) ++
        text "in" ++
        indent (showBasexp basexp) ++
        text "end"
      | showBasexp (Var basid) =
        text basid
    and showBasdec (Basis basbinds) =
        showBasbinds basbinds
      | showBasdec (Local (basdecs, basdecs')) =
        text "local" ++
        indent (showBasdecs basdecs) ++
        text "in" ++
        indent (showBasdecs basdecs') ++
        text "end"
      | showBasdec (Include file) =
        text ("\"" ^ file ^ "\"")
      | showBasdec (Source file) = 
        text ("\"" ^ file ^ "\"")
      | showBasdec (Open basids) =
        text (foldl (fn (basid, s) => s ^ " " ^ basid) "open" basids)
    and showBasbind (basid, basexp) =
        text ("basis " ^ basid ^ "=") ++
        indent (showBasexp basexp)
    and showBasdecs [basdec] =
        showBasdec basdec
      | showBasdecs (basdec :: basdecs) =
        showBasdec basdec ++
        showBasdecs basdecs
      | showBasdecs _ = text ""
    and showBasbinds [basbind] =
        showBasbind basbind
      | showBasbinds (basbind :: basbinds) =
        let
            fun showBasbind' (basid, basexp) =
                text ("  and " ^ basid ^ "=") ++
                indent (showBasexp basexp)
            fun showBasbinds' [basbind] =
                showBasbind' basbind
              | showBasbinds' (basbind :: basbinds) =
                showBasbind' basbind ++
                showBasbinds' basbinds
              | showBasbinds' _ = Crash.impossible "MLBGrammar.showBasbinds'"
        in
            showBasbind basbind ++
            showBasbinds' basbinds
        end
      | showBasbinds _ = Crash.impossible "MLBGrammar.showBasbinds"
    end
end
