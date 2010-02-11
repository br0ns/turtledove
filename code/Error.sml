structure Error :> Error =
struct
exception Error of {pos: int, msg: string}
fun error p s = raise Error {pos = p, msg = s}
end
