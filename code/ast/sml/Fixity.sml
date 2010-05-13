structure Fixity :> Fixity =
struct
datatype t = Nonfix | InfixL of int option | InfixR of int option | Op
end
