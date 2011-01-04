signature Variable =
sig
  type t
  val ofIdent : Ident.t -> t
  val ident : t -> Ident.t
  val load : t -> ValEnv.vid
  val store : t -> ValEnv.vid -> t

  val toString : t -> string
end
