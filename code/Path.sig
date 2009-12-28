signature Path =
sig
  eqtype t

  exception Path of Report.t

  (* Raises Path if the argument is a relative path. *)
  val new : string -> t

  (* Relative to first argument.
     Ignores first argument if second argument is an absolute path. *)
  val new' : t -> string -> t

  val path : t -> string
  (* Alias *)
  val toString : t -> string
  (* Relative to first argument *)
  val path' : t -> t -> string

  val file : t -> string
  val dir : t -> t
  val base : t -> string
  val extension : t -> string option

  val exists : t -> bool
  val readable : t -> bool
  val writable : t -> bool

  val show : t -> Report.t
end
