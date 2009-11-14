structure Crash : Crash =
struct
    exception Crash

    fun assert (msg, cond) =
        if cond then
            ()
        else
            let
                val _ =
                    if not cond andalso Flags.get "Debug.Assert" then
                        (print "!! Assertion failed:\n" ;
                         print ("!! " ^ msg ^ "\n"))
                    else
                        ()
            in
                raise Crash
            end

    fun impossible msg =
        let
            val _ = 
                if Flags.get "Debug.Impossible" then
                    (print "!! Something impossible happened:\n" ;
                     print ("!! " ^ msg ^ "\n"))
                else
                    ()
        in
            raise Crash
        end
        
    fun unimplemented msg =
        let
            val _ =
                if Flags.get "Debug.Unimplemented" then
                    (print "!! Not implemented:\n" ;
                     print ("!! " ^ msg ^ "\n"))
                else
                    ()
        in
            raise Crash
        end

    fun debug msg =
        if Flags.get "Debug" then
            print ("-- " ^ msg ^ "\n")
        else
            ()
end
