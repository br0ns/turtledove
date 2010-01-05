datatype syntax = Ident of Ident.t
                | Decs

type data = {syntax : syntax, info : Info.t}

info {position : Position.t, environmet : Environment.t}
