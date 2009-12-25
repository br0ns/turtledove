(* TODO: Review with regard to symbolic links. As it is now, it almost certainly
   doesn't work *)

structure Path :> Path =
struct
structure P = OS.Path
structure FS = OS.FileSys

(* Invariant: Values of type t represent absolute canonical paths *)
type t = string

exception Path

fun path f = f
val toString = path

fun new f =
    if P.isAbsolute f then
      P.mkCanonical f
    else
      raise Path

fun new' f f' =
    if P.isAbsolute f' then
      new f'
    else
      new (P.concat (f, f'))
    
fun path' f f' = P.mkCanonical (P.mkRelative {path = f', relativeTo = f})

val file = P.file
val dir = P.dir
val base = P.base
val extension = P.ext

fun exists f = FS.access (f, nil)
fun readable f = FS.access (f, [FS.A_READ])
fun writable f = FS.access (f, [FS.A_WRITE])

val size = Position.toInt o FS.fileSize
val modtime = FS.modTime

val show = Report.text

val openIn = TextIO.openIn
val openOut = TextIO.openOut
val openAppend = TextIO.openAppend
end
