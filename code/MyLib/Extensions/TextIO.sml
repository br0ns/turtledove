structure TextIO :> TextIO =
struct
open TextIO

fun println s = (print s ; print "\n")
end
