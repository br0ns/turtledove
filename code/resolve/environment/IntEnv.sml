structure IntEnv =
struct
datatype int = I of {vi : StringSet.t,
                     ti : StringSet.t Dictionary.t,
                     si : int Dictionary.t}
type si = int Dictionary.t
type fi = (int * int) Dictionary.t

type t = si * fi

end
