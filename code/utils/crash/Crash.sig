(* TODO: Update the project and start use this in stead of any "die", "unimp" or
   other kind of locally defined functions with the same kind of functionality
 *)

(* Handle different kind of errors. Mostly for debugging. *)

signature Crash =
sig
    (* Throw on error *)
    exception Crash

    (* Throws Crash if (lazy) condition is false *)
    val assert : string * (unit -> bool) -> unit

    (* For things that "can't" happen *)
    val impossible : string -> 'a

    (* Create a 'die' function (a function that prepends a string to its
       argument and calls impossible *)
    val die : string -> string -> 'a

    val unimplemented : string -> 'a

    (* Doesn't throw Crash, just prints to StdOut *)
    val debug : string -> unit
end
