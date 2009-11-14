signature Report =
sig
    type report

    val text : string -> report
    val indent : report -> report
    val verbatim : string -> report
    val ++ : report * report -> report
    val toString : report -> string
end
