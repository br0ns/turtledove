structure MLBGrammarUtils :> MLBGrammarUtils =
struct
    val showBId = Ident.showId
    val showLongBId = Ident.showLongId
    fun showFile f = f

    val ind = Utils.indent Constants.TAB_WIDTH

    fun showBDec bdec =
        case bdec of
            Seq_bdec (bdec1, bdec2) =>
            "Seq_bdec (\n" ^
            ind showBDec bdec1 ^ ",\n" ^
            ind showBDec bdec2 ^
            "\n)"
          | Empty_bdec =>
            "Empty_bdec"
          | Local_bdec (bdec1, bdec2) => 
            "Local_bdec (\n" ^
            ind (showBDec bdec1) ^ ",\n" ^
            ind (showBDec bdec2) ^
            "\n)"
          | Basis_bdec (bid, bexp) =>
            "Basis_bdec (\n" ^
            ind (showBId bid) ^ ",\n" ^
            ind (showBExp bexp) ^
            "\n)"
          | Open_bdec (lbids) =>
            "Open_bdec " ^ Show.list showLongBId lbids
          | Source_bdec f =>
            "Source_bdec (\"" ^ showFile f ^ "\")"
          | Include_bdec f =>
            "Include_bdec (\"" ^ showFile f ^ "\")"
    and showBExp bexp =
        case bexp of
            Dec_bexp bdec =>
            "Dec_bexp (\n" ^
            ind (showBDec bdec) ^
            "\n)"
          | Let_bexp (bdec, bexp) =>
            "Let_bexp (\n" ^
            ind (showBDec bdec) ^ ",\n" ^
            ind (showBExp bexp) ^
            "\n)"
          | LongBid_bexp lbid => 
            "LongBid_bexp (" ^ showLongBId lbid ^ ")"
end
