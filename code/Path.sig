signature Path =
sig
  eqtype t

  exception Path

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

  (* val copy : {from : t, to : t} -> unit *)
  (* val move : {from : t, to : t} -> unit *)
  (* val create: t -> unit (\* make an empty file if file does not exists *\) *)
  (* val remove : t -> unit *)

  (* val temp : unit -> t *)

  (* val contents : t -> string *)
  val size : t -> int (* in bytes *)
  val modtime : t -> Time.time

  val show : t -> Report.t

  val openIn : t -> TextIO.instream
  val openOut : t -> TextIO.outstream
  val openAppend : t -> TextIO.outstream

  (* val withIn : t -> (TextIO.instream -> 'a) -> 'a *)
  (* val withOut : t -> (TextIO.outstream -> 'a) -> 'a *)
  (* val withAppend : t -> (TextIO.outstream -> 'a) -> 'a *)
end
