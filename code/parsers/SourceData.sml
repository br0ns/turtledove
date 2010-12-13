structure SourceData :> SourceData =
struct
type start_pos = int
type depth = int
type comment = start_pos * string
type t = {C: depth ref * comment list ref,
          S: start_pos ref * string ref}

fun new _ = {C = (ref 0, ref nil), S = (ref 0, ref "")}

structure Comments =
struct
type source_data = t
type t = comment list
fun die s = Crash.impossible ("LexUtils.Comments: " ^ s)

fun new ({C = (d, cs), ...} : source_data) p =
    (d := !d + 1 ; cs := (p, "") :: !cs)

fun inc ({C = (d, _), ...} : source_data) =
    d := !d + 1

fun dec ({C = (d, _), ...} : source_data) =
    d := !d - 1

fun append ({C = (_, cs), ...} : source_data) s =
    case !cs of
      (p, c) :: r => cs := (p, c ^ s) :: r
    | _ => die "append: No comments opened."

fun get ({C = (_, cs), ...} : source_data) = rev (!cs)

fun start ({C = (_, cs), ...} : source_data) =
    case !cs of
      (p, _) :: _ => p
    | _ => die "start: No comments opened."

fun depth ({C = (d, _), ...} : source_data) = !d
end

structure String =
struct
type source_data = t
fun die s = Crash.impossible ("LexUtils.String: " ^ s)
fun new ({S = (p, s), ...} : source_data) p' = (p := p' ; s := "")

fun append ({S = (_, s), ...} : source_data) cs = s := !s ^ cs
fun appendChar s = append s o str

fun asControlChar s fail =
    case explode s of
      [#"\\", c] =>
      let
        val c = ord c
      in
        if c >= ord #"@" andalso c <= ord #"_" then
          chr (c - ord #"@")
        else
          (fail ("Illegal control escape; must be one of " ^
                 "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_") ;
           #" " (* Dummy *)
          )
      end
    | _ => die "asControlChar"

fun charToInt c =
    if Char.isDigit c then
      ord c - ord #"0"
    else if Char.isUpper c then
      ord c - ord #"A" + 10
    else if Char.isLower c then
      ord c - ord #"a" + 10
    else
      die "charToInt"

fun charsToInt base =
    foldl (fn (c, a) => a * base + charToInt c) 0

fun asAsciiChar s fail =
    case explode s of
      [#"\\", c1, c2, c3] =>
      let
        val c = charsToInt 10 [c1, c2, c3]
      in
        if c > 255 then
          (fail ("Illegal ASCII escape: " ^ Int.toString c) ;
           #" " (* Dummy *)
          )
        else
          chr c
      end
    | _ => die "asAsciiChar"

fun asUnicodeChar s fail =
    let
      val c =
          case explode s of
            #"\\" :: #"u" :: cs => charsToInt 16 cs
          | #"\\" :: #"U" :: cs => charsToInt 32 cs
          | _ => die "asUnicodeChar"
    in
      if c > 255 then
        (* TODO: Some unicode thingie. Migrate to WideString maybe. *)
        (Crash.debug "Unicode escape > 255. Returned #\"X\"." ;
         #"X"
        )
      else
        chr c
    end

fun appendControlChar s c fail = appendChar s (asControlChar c fail)
fun appendAsciiChar s c fail = appendChar s (asAsciiChar c fail)
fun appendUnicodeChar s c fail = appendChar s (asUnicodeChar c fail)

fun get ({S = (_, s), ...} : source_data) = !s
fun start ({S = (p, _), ...} : source_data) = !p
end
end
