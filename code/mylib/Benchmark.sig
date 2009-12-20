signature Benchmark =
sig
    val start : unit -> unit
    val pause : unit -> unit
    val stop : unit -> unit

    (* Equivalent to stop() ; start () *)
    val restart : unit -> unit

    val show : unit -> Report.t

    (* print = Report.print o show *)
    val print : unit -> unit
end
