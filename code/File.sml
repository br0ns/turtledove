structure File :> File =
struct
type t = Path.t

structure FS = OS.FileSys

val size = Position.toInt o FS.fileSize o Path.path
val modtime = FS.modTime o Path.path

val openIn = TextIO.openIn o Path.path
val openOut = TextIO.openOut o Path.path
val openAppend = TextIO.openAppend o Path.path

end
