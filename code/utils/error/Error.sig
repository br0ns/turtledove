(* TODO: Error handling is a mess throughout Turtledove. Somebody^TM should come
 * up with a scheme, and use that everywhere. *)
signature Error =
sig
  exception Internal of {pos: int, msg: string}
  exception External of {file: File.t, pos: int, msg: string}
  val internal : int -> string -> 'a
  val external : File.t -> int -> string -> 'a
end
