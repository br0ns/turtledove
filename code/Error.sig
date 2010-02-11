signature Error =
sig
  exception Error of {pos: int, msg: string}
  val error : int -> string -> 'a
end
