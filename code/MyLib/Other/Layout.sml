structure Layout :> Layout =
struct
open General
open Pretty infix ^^ ++ \ & \\ &&

val chr = txt o str
val int = txt o Show.int
val real = txt o Show.real
val bool = txt o Show.bool
fun option f = txt o Show.option f

fun spaces n = txt (String.tabulate (n, fn _ => #" "))

val lparen = txt "("
val rparen = txt ")"
val langle = txt "<"
val rangle = txt ">"
val lbrace = txt "{"
val rbrace = txt "}"
val lbracket = txt "["
val rbracket = txt "]"
val pling = txt "'"
val quote = txt "\""
val semi = txt ";"
val colon = txt ":"
val comma = txt ","
val space = txt " "
val dot = txt "."
val dash = txt "-"
val sharp = txt "#"
val percent = txt "%"
val dollar = txt "$"
val ampersand = txt "&"
val slash = txt "/"
val backslash = txt "\\"
val eq = txt "="

fun punctuate sep ds =
    let
      fun loop nil = nil
        | loop [d] = [d]
        | loop (d :: ds) = (d ^^ sep) :: loop ds
    in
      loop ds
    end

fun align d = column (fn k => nesting (fn i => nest (k - i) d))
fun hang i d = align (nest i d)
fun indent i d = hang i (spaces i ^^ d)
fun width f d = column (fn l => d ^^ column (fn r => f (r - l)))

fun fill f =
    width (fn w =>
              if f > w then
                spaces (f - w)
              else
                empty
          )
fun fillBreak f =
    width (fn w =>
              if f >= w then
                spaces (f - w)
              else
                nest f brk
          )

val softln = group ln
val softbrk = group brk

local
  fun mk sep (l, r) = l ^^ sep ^^ r
in
val op++ = mk space
val op\  = mk ln
val op&  = mk softln
val op\\ = mk brk
val op&& = mk softbrk
end

local
  fun mk sep ds =
      case rev ds of
        nil     => empty
      | d :: ds => foldl sep d ds
in
val hsep    = mk op++
val vsep    = mk op\
val fillSep = mk op&
val hcat    = mk op^^
val vcat    = mk op\\
val fillCat = mk op&&
end

val sep = group o vsep
val cat = group o vcat

fun enclose (l, r) d = l ^^ d ^^ r
val plings   = enclose (pling, pling)
val quotes   = enclose (quote, quote)
val parens   = enclose (lparen, rparen)
val angles   = enclose (langle, rangle)
val braces   = enclose (lbrace, rbrace)
val brackets = enclose (lbracket, rbracket)

fun str s =
    let
      datatype bullet = Text of string
                      | Numbered of string * int * string
      fun bullet s =
          let
            fun or nil ss = NONE
              | or (r :: rs) ss =
                case r ss of
                  SOME x => SOME x
                | NONE   => or rs ss

            fun cat rs ss =
                let
                  fun loop (nil, ss) = SOME ("", ss)
                    | loop (r :: rs, ss) =
                      case r ss of
                        NONE => NONE
                      | SOME (s, ss) =>
                        case loop (rs, ss) of
                          NONE => NONE
                        | SOME (s', ss) => SOME (s ^ s', ss)
                in
                  loop (rs, ss)
                end

            val number = Int.scan StringCvt.DEC Substring.getc
            fun string s ss =
                let
                  fun loop (ss, nil) = SOME (s, ss)
                    | loop (ss, c :: cs) =
                      case Substring.getc ss of
                        SOME (c', ss') =>
                        if c' = c then
                          loop (ss', cs)
                        else
                          NONE
                      | NONE => NONE
                in
                  loop (ss, explode s)
                end
            fun ws ss =
                let
                  fun loop ss' =
                      case Substring.getc ss' of
                        SOME (c, ss') =>
                        if c = #" " then
                          case loop ss' of
                            SOME (s, ss') => SOME (" " ^ s, ss')
                          | NONE => SOME (" ", ss')
                        else
                          SOME ("", ss)
                      | NONE => NONE
                in
                  loop ss
                end
            val empty = string ""
            val AD = string "AD"
            val Ad = string "Ad"
            val ad = string "ad"
            val dot = string "."
            val colon = string ":"

            fun numbered ss =
                case cat [or [AD, Ad, ad, empty], ws] ss of
                  NONE => NONE
                | SOME (l, ss) =>
                  case number ss of
                    NONE => NONE
                  | SOME (n, ss) =>
                    case or [cat [ws, or [dot, colon]], empty] ss of
                      NONE => NONE
                    | SOME (r, ss) =>
                      SOME (Numbered (l, n, r), ss)

            fun text ss =
                case Substring.getc ss of
                  SOME (c, ss) =>
                  if c = #"o" orelse Char.isPunct c then
                    SOME (Text (String.str c), ss)
                  else
                    NONE
                | NONE => NONE
          in
            case or [numbered, text] s of
              SOME (b, s) => (SOME b, s)
            | NONE        => (NONE, s)
          end
      fun next (Text s) = Text s
        | next (Numbered (l, n, r)) = Numbered (l, n + 1, r)
      fun bulletToString b =
          case b of
            Text s => s
          | Numbered (l, n, r) => l ^ Int.toString n ^ r

      datatype t = Par of int * substring
                 | List of int * (string * t list) list

      fun trees ls =
          let
            fun loop (_, nil) = (nil, nil)
              | loop (i', (i, l) :: ls) =
                if i' <= i then
                  case bullet l of
                    (NONE, _) =>
                    let
                      val (ts, ls) = loop (i', ls)
                    in
                      (Par (i - i', l) :: ts, ls)
                    end
                  | (SOME b, _) =>
                    let
                      fun loopi (_, _, nil) = (nil, nil)
                        | loopi (i'', b', (i, l) :: ls) =
                          case (i'' = i, bullet l) of
                            (true, (SOME b, l)) =>
                            let
                              val b = if b = Text "." then next b' else b
                              val bs = bulletToString b
                              val (item, ls) = loop (i + size bs + 1, ls)
                              val (items, ls) = loopi (i, b, ls)
                            in
                              ((bs, Par (0, l) :: item) :: items, ls)
                            end
                          | _ => (nil, (i, l) :: ls)
                      val (lst, ls) = loopi (i, b, (i, l) :: ls)
                      val (ts, ls) = loop (i', ls)
                    in
                      (List (i - i', lst) :: ts, ls)
                    end
                else
                  (nil, (i, l) :: ls)
            val (ts, _) = loop (0, ls)
          in
            ts
          end

      fun softtxt s =
          (fillSep o
           map (txt o Substring.string) o
           Substring.fields Char.isSpace
          ) s

      fun doc ts =
          let
            fun one (Par (i, s)) =
                spaces i ^^ nest i (softtxt s)
              | one (List (i, items)) =
                let
                  val width = foldl Int.max 0 (map (size o Pair.fst) items)
                in
                  vcat
                    (map
                       (fn (b, ts) =>
                           spaces i ^^ (fill width (txt b)) ^^
                                  nest (width + i + 1) (many ts)
                       ) items
                    )
                end
            and many ts = vcat (map one ts)
          in
            many ts
          end
    in
      (doc o
       trees o
       map (Pair.map (Substring.size, id) o
            Substring.splitl Char.isSpace) o
       Substring.fields (curry op= #"\n") o
       Substring.full) s
    end

end
