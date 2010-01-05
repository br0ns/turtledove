(* TODO: Review with regard to symbolic links. As it is now, it almost certainly
   doesn't work.
   Read path variables from a file
*)

structure Path :> Path =
struct
structure P = OS.Path

(* Invariant: Values of type t represent absolute canonical paths *)
type t = string

exception Path of Report.t

val vars = Dictionary.fromList
             [("SML_LIB", "/usr/lib/mlton/sml"),
              ("OBJPTR_REP", "objptr-rep32.sml"),
              ("HEADER_WORD", "header-word32.sml"),
              ("SEQINDEX_INT", "seqindex-int32.sml"),
              ("TARGET_ARCH", "x86"),
              ("TARGET_OS", "linux"),
              ("DEFAULT_CHAR", "default-char8.sml"),
              ("DEFAULT_WIDECHAR", "default-widechar32.sml"),
              ("DEFAULT_INT", "default-int32.sml"),
              ("DEFAULT_REAL", "default-real64.sml"),
              ("DEFAULT_WORD", "default-word32.sml")
             ]

fun expandVars f =
    let
      fun read (#"$" :: #"(" :: cs, seen) =
          let
            val (var, cs) = readVar cs
            val var = implode var
          in
            if StringSet.member seen var then
              raise Path (Report.text ("Recursive path variable: " ^ var))
            else
              case Dictionary.lookup vars var of
                SOME path => read (explode path, StringSet.insert seen var) @
                             read (cs, seen)
              | NONE => raise Path (Report.text ("Unknown path variable: " ^ var))
          end
        | read (c :: cs, seen) = c :: read (cs, seen)
        | read _ = nil
      and readVar (#")" :: cs) = (nil, cs)
        | readVar (c :: cs) =
          let
            val (cs, cs') = readVar cs
          in
            (c :: cs, cs')
          end
        | readVar _ = (nil, nil)
    in
      implode (read (explode f, StringSet.empty))
    end

fun path f = f
val toString = path

fun new f =
    let
      val f = expandVars f
    in
      if P.isAbsolute f then
        P.mkCanonical f
      else
        raise Path (Report.text "Cannot create a relative path.")
    end

fun new' f f' =
    let
      val f' = expandVars f'
    in
      if P.isAbsolute f' then
        new f'
      else
        new (P.concat (f, f'))
    end

fun path' f f' = P.mkCanonical (P.mkRelative {path = f', relativeTo = f})

val file = P.file
val dir = P.dir
val base = P.base
val extension = P.ext

val sub = String.isPrefix

val show = Report.text
end
