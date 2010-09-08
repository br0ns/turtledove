structure Error :> Error =
struct
exception Internal of {pos: int, msg: string}
exception External of {file: File.t, pos: int, msg: string}
fun internal p s = raise Internal {pos = p, msg = s}
fun external f p s = raise External {file = f, pos = p, msg = s}
end
