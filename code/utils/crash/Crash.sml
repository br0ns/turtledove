structure Crash : Crash =
struct
    exception Crash

    fun assert (msg, cond) =
        (* andalso short circuits so cond is only evaluated when assertions are
           on *)
        if Flags.get "Debug.Assert" andalso not (cond ()) then
            (print ("\n!! Assertion failed:\n!! " ^ msg ^ "\n") ;
             raise Crash
            )
        else
            ()

    fun impossible msg = (
        (if Flags.get "Debug.Impossible" then
            print ("\n!! Something impossible happened:\n!! " ^ msg ^ "\n")
         else
             ()) ;
        raise Crash
    )

    fun die s s' = impossible (s ^ ": " ^ s')

    fun unimplemented msg = (
        (if Flags.get "Debug.Unimplemented" then
             print ("\n!! Not implemented:\n!! " ^ msg ^ "\n")
         else
             ()) ;
        raise Crash
    )

    fun debug msg =
        if Flags.get "Debug" then
            print ("-- " ^ msg ^ "\n")
        else
            ()
end
