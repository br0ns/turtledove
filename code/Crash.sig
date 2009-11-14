(* Handle different kind of errors. Mostly for debugging. *)

signature Crash =
sig
    exception Crash                       (* Throw on error *)

    val assert : (string * bool) -> unit  (* Throws Crash if condition is false *)
    val impossible : string -> 'a         (* For things that "can't" happen *)
    val unimplemented : string -> 'a
    val debug : string -> unit            (* Doesn't throw Crash *)
end
