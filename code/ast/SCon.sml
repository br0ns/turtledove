structure SCon :> SCon =
struct
datatype t = String of string
           | Char of string
           | Int of string
           | Real of string
           | Word of string
fun toString scon =
    case scon of
      String x => x
    | Char x => x
    | Int x => x
    | Real x => x
    | Word x => x
end
