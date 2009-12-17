structure MLBGrammar =
struct
    type bid = Ident.id
    type longbid = Ident.longid
    type file = string

    datatype bdec =
             Seq_bdec of bdec * bdec
           | Empty_bdec
           | Local_bdec of bdec * bdec
           | Basis_bdec of bid * bexp
           | Open_bdec of longbid list
           | Source_bdec of file
           | Include_bdec of file

    and      bexp =
             Dec_bexp of bdec
           | Let_bexp of bdec * bexp
           | LongBid_bexp of longbid
end
