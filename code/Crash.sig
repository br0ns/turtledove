(* Handle different kind of errors. Mostly for debugging. *)

signature Crash =
sig
    (* Throw on error *)
    exception Crash

    (* Throws Crash if (lazy) condition is false *)
    val assert : string * (unit -> bool) -> unit

    (* For things that "can't" happen *)
    val impossible : string -> 'a

    val unimplemented : string -> 'a

    (* Doesn't throw Crash, just prints to StdOut *)
    val debug : string -> unit
end
