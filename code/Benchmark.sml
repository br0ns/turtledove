structure Benchmark :> Benchmark =
struct
    val zeroTime = {usr = Time.zeroTime, sys = Time.zeroTime}
    val dummyTimer = Timer.totalCPUTimer ()

    val time = ref zeroTime
    val time' = ref zeroTime
    val timer = ref dummyTimer

    fun ++ {usr = u1, sys = s1} =
        let
            val {usr = u2, sys = s2} = !time
        in
            time := {usr = Time.+ (u1, u2), sys = Time.+ (s1, s2)}
        end

    fun start () = timer := Timer.startCPUTimer ()
    fun pause () = (++ (Timer.checkCPUTimer (!timer)) ; time' := !time)
    fun stop () = (pause () ; time := zeroTime)
    fun restart () = (stop () ; start ())
    fun show () =
        let
            open Report
            infix ++
            val {usr, sys} = !time'
        in
            text ("User: " ^ Time.toString usr ^ ", " ^
                  "System: " ^ Time.toString sys ^ ", " ^
                  "Total: " ^ Time.toString (Time.+ (usr, sys))
                 )
        end

    fun print "" = Report.print (show ())
      | print s = Report.print (Report.++ (Report.text s, Report.indent (show ())))
    fun stopAndPrint s = (stop () ; print s)
end
