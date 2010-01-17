
(* NEARLY FINISHED *)

structure Report :> Report =
struct
structure FloatH =
struct datatype t = Left | Center | Right end

structure FloatV =
struct datatype t = Top | Center | Bottom end

structure Format =
struct datatype t = Plain | Paragraph | Indent end

structure BorderStyle =
struct datatype t = StyleA | StyleB | StyleC end

structure EnumStyle =
struct datatype t = Number | Letter | Roman end

structure Side =
struct datatype t = Left | Right | Top | Bottom end

structure Heading =
struct datatype t = Both | Vertical | Horizontal end

exception Width

datatype t = Text      of {floath      : FloatH.t,
                           floatv      : FloatV.t,
                           format      : Format.t,
                           contents    : string
                          }
           | Row       of t list
           | Col       of t list
           | Fill
           | SpaceH    of int
           | SpaceV    of int
           | Indent    of {indentation : int,
                           contents    : t
                          }
           | Itemize   of {bullet      : string option,
                           items       : t list
                          }
           | Enumerate of {style       : EnumStyle.t,
                           items       : t list
                          }
           | Border    of {borderstyle : BorderStyle.t,
                           sides       : Side.t list,
                           contents    : t
                          }
           | Table     of {borderstyle : BorderStyle.t,
                           heading     : Heading.t,
                           items       : t list list
                          }

val BIG_INDENT = Constants.TAB_WIDTH
val SMALL_INDENT = BIG_INDENT div 2
val BULLETS = ["*", "o", "-", ".", "#", "="]
val LINE_LENGTH = Constants.LINE_LENGTH

fun die s = raise Fail ("Report: " ^ s)

fun spaces n = List.tabulate (n, fn _ => #" ")

local (* BEGIN textToRect : FloatH.t -> Format.t -> int -> string -> rect *)
  datatype token = Space of int | Word of char list
  fun tsize (Space n) = n
    | tsize (Word w) = length w

  local (* BEGIN tokenize : string -> token list *)
    fun readSpace cs =
        let
          fun loop (#" " :: cs, n) = loop (cs, n + 1)
            | loop (cs, n) = (Space n, cs)
        in
          loop (cs, 0)
        end
    fun readWord cs =
        let
          fun loop (nil, n) = (nil, nil)
            | loop (c :: cs, n) =
              case c of
                #" " => (nil, c :: cs)
              | #"-" => ([c], cs)
              | #"_" => ([c], cs)
              | #"." => ([c], cs)
              | #"," => ([c], cs)
              | _ =>
                let
                  val (w, cs) = loop (cs, n + 1)
                in
                  (c :: w, cs)

                end
          val (w, cs) = loop (cs, 0)
        in
          (Word w, cs)
        end
    fun read (cs as #" " :: _) = readSpace cs
      | read cs = readWord cs
  in
  fun tokenize s =
      let
        fun loop nil = nil
          | loop cs =
            let
              val (t, cs) = read cs
            in
              t :: loop cs
            end
      in
        loop (explode s)
      end
  end (* END tokenize *)

  fun line maxw ts =
      let
        fun breakWord w =
            let
              fun loop (nil, _) = (nil, nil)
                | loop (w, 0) = (nil, w)
                | loop (c :: cs, n) =
                  let
                    val (w, w') = loop (cs, n - 1)
                  in
                    (c :: w, w')
                  end
            in
              loop (w, maxw)
            end
        fun loop (nil, l) = (nil, nil)
          | loop (t :: ts, l) =
            let
              val tsize = tsize t
              val l = l + tsize
            in
              if l > maxw then
                case t of
                  (* The line ends in a space, so just drop it *)
                  Space _ => (nil, ts)
                  (* The line ends in a word *)
                | Word w  =>
                  (* Is the word longer than the maximum width? *)
                  if tsize > maxw then
                    let
                      val (w, w') = breakWord w
                    in
                      if l = tsize then
                        (* There is space on this line *)
                        ([Word w], Word w' :: ts)
                      else
                        (* Put it on the next line *)
                        (nil, Word w :: Word w' :: ts)
                    end
                  else
                    (nil, t :: ts)
              else
                let
                  val (ts, ts') = loop (ts, l)
                in
                  (t :: ts, ts')
                end
            end
      in
        loop (ts, 0)
      end

  fun lines maxw nil = nil
    | lines maxw ts =
      let
        val (l, ls) = line maxw ts
      in
        l :: lines maxw ls
      end

  local (* BEGIN rect : FloatH.t -> Format.t -> int -> token list list -> char list list *)
    open FloatH Format

    fun stripLeft (Space _ :: ts) = stripLeft ts
      | stripLeft ts = ts
    val stripRight = rev o stripLeft o rev
    val strip = stripRight o stripLeft
    fun untokenize nil = nil
      | untokenize (Space n :: ts) = spaces n @ untokenize ts
      | untokenize (Word w :: ts) = w @ untokenize ts
  in
  fun rect float format maxw ls =
      let
        fun padLeft l = spaces (maxw - length l) @ l
        fun padRight l = l @ spaces (maxw - length l)
        fun padBoth l =
            let
              val spc = maxw - length l
            in
              spaces (spc div 2) @ l @ spaces ((spc + 1) div 2)
            end
        fun stripSpace nil = nil
          (* Don't strip spaces if theres only one line *)
          | stripSpace [l] = [l]
          | stripSpace (l :: ls) =
            let
              fun loop [l] =
                  (* Last line. Preserve final space *)
                  [stripLeft l]
                | loop (l :: ls) = strip l :: loop ls
                | loop _ = nil
            in
              (* Give first line special treatment as well *)
              stripRight l :: loop ls
            end

        fun indent (l :: ls) =
            (* Indent first line if in paragraph mode *)
            (case (format, float) of
               (Paragraph, Left)  => Space SMALL_INDENT :: l
             | (Paragraph, Right) => l @ [Space SMALL_INDENT]
             | _                  => l) ::
            (* Indent the rest of the lines if in indent mode *)
            map (fn l =>
                    case (format, float) of
                      (Indent, Left)  => Space SMALL_INDENT :: l
                    | (Indent, Right) => l @ [Space SMALL_INDENT]
                    | _               => l
                ) ls
          | indent _ = nil
      in
        (* If there is only one line, don't pad it with spaces *)
        (map (case ls of
                [_] => (fn x => x)
              | _   => case float of
                         Left   => padRight
                       | Right  => padLeft
                       | Center => padBoth
             ) o map untokenize o indent o stripSpace) ls
      end
  end (* END rect *)
in
fun textToRect float format maxw s =
    let
      open Format

      val plain = lines maxw
      fun paragraph ts =
          let
            val (l, ls) = line (maxw - SMALL_INDENT) ts
          in
            l :: lines maxw ls
          end
      fun indent ts =
          let
            val (ts, ts') = line maxw ts
          in
            ts :: lines (maxw - SMALL_INDENT) ts'
          end
    in
      (rect float format maxw o
       (case format of
          Plain     => plain
        | Paragraph => paragraph
        | Indent    => indent) o
       tokenize) s
    end
end (* END textToRect *)

(* Intermediate format. Text nodes are converted to rectangles. SpaceHs are
   truncated *)
structure Inter =
struct
type rect = char list list
datatype t = Rect      of {floath      : FloatH.t,
                           floatv      : FloatV.t,
                           contents    : rect}
           | Row       of t list
           | Col       of t list
           | Fill
           | SpaceH    of int
           | SpaceV    of int
           | Indent    of {indentation : int,
                           contents    : t
                          }
           | Itemize   of {bullet      : string,
                           items       : t list
                          }
           | Enumerate of {bullets     : char list list * int,
                           items       : t list}
           | Border    of {borderstyle : BorderStyle.t,
                           sides       : Side.t list,
                           contents    : t
                          }
           | Table     of {borderstyle : BorderStyle.t,
                           heading     : Heading.t,
                           items       : t list list
                          }
end

local (* BEGIN toRects : string list -> int -> Inter.t -> t *)
  local (* BEGIN enumbullets : EnumStyle.t -> int -> char list list *)
    open EnumStyle
    fun number n = Int.toString n ^ "."
    fun letter n = List.nth (["a", "b", "c", "d",
                              "e", "f", "g", "h",
                              "i", "j", "k", "l",
                              "m", "n", "o", "p",
                              "q", "r", "s", "t",
                              "u", "v", "w", "x",
                              "y", "z"], n - 1) ^ "."
    local
      fun toChars xs =
          map (fn x =>
                  case x of
                    1000 => #"M"
                  |  500 => #"D"
                  |  100 => #"C"
                  |   50 => #"L"
                  |   10 => #"X"
                  |    5 => #"V"
                  |    1 => #"I"
                  | _    => die "enumbullets.roman"
              ) xs
      fun toRoman x =
          let
            val rs = [1000, 500, 100, 50, 10, 5, 1]
            val rsr = rev rs
            fun subtract (x, y :: ys) =
                if x + y >= 0 then
                  y
                else
                  subtract (x, ys)
              | subtract _ = die "enumbullets.roman"

            fun loop (0, _) = nil
              | loop (x, yl as y :: ys) =
                if x >= y then
                  y :: loop (x - y, yl)
                else
                  (* Don't ask - it works *)
                  if x >= y * (9 - y div hd ys mod 2) div 10 then
                    let
                      val z = subtract (x - y, rsr)
                    in
                      z :: y :: loop (x - y + z, ys)
                    end
                  else
                    loop (x, ys)
              | loop _ = die "enumbullets.roman"
          in
            loop (x, rs)
          end
    in
    fun roman n = (implode o toChars o toRoman) n ^ "."
    end
  in
  fun enumbullets style n =
      let
        val ns = List.tabulate
                     (n, fn n =>
                            (case style of
                               Number => number
                             | Letter => letter
                             | Roman  => roman
                            ) (n + 1)
                     )
        val w = (foldl Int.max 0 o map size) ns
      in
        (map (fn n => spaces (w - size n + 1) @ explode n @ [#" "]) ns,
         w)
      end
  end (* END enumbullets *)
in
fun toRects bullets maxw r =
    if maxw <= 0 then
      raise Width
    else
      case r of
        Text {floath, floatv, format, contents} =>
        Inter.Rect {floath = floath,
                    floatv = floatv,
                    contents = textToRect floath format maxw contents
                   }
      | Row nil => Inter.Row nil
      | Row rs =>
        let
          val n = length rs
          val (space, spaces) =
              foldl (fn (SpaceH w, (s, n)) => (s + w, n + 1)
                      | (SpaceV h, (s, n)) => (s, n + 1)
                      | (_, x)             => x
                    ) (0, 0) rs
          val maxw = maxw - space
          val n = n - spaces
        in
          if n > 0 then
            Inter.Row (map (toRects bullets (maxw div n)) rs)
          else

            (* It's a row of spacing, any maxw will do if there isn't
               enough space maxw will be negative and trigger an error
             *)
            Inter.Row (map (toRects bullets maxw) rs)
        end
      | Col rs =>
        Inter.Col (map (toRects bullets maxw) rs)
      | Fill => Inter.Fill
      | SpaceH w => Inter.SpaceH w
      | SpaceV h => Inter.SpaceV h
      | Indent {indentation, contents} =>
        Inter.Indent {indentation = indentation,
                      contents = toRects bullets (maxw - indentation) contents
                     }
      | Itemize {bullet, items} =>
        let
          val (bullet, bullets) =
              case (bullet, bullets) of
                (SOME b, bs)    => (b, bs)
              | (NONE, b :: bs) => (b, bs)
              | (NONE, nil)     => (hd BULLETS, tl BULLETS)
          val maxw = maxw - size bullet - 2
        in
          Inter.Itemize {bullet = bullet,
                         items = map (toRects bullets maxw) items
                        }
        end
      | Enumerate {style, items} =>
        let
          val (bs, w) = enumbullets style (length items)
          val maxw = maxw - w
        in
          Inter.Enumerate {bullets = (bs, w),
                           items = map (toRects bullets maxw) items
                          }
        end
      | _ => die "toRects"
end (* END toRects *)

local (* BEGIN toStringWithMaxWidth : int -> t -> string *)
  open Inter
  val rectWidth = length o hd
  val rectHeight = length

  fun padLeft 0 ls = ls
    | padLeft n ls =
      let
        val space = spaces n
      in
        map (fn l => space @ l) ls
      end
  fun padRight 0 ls = ls
    | padRight n ls =
      let
        val space = spaces n
      in
        map (fn l => l @ space) ls
      end
  fun padTop 0 ls = ls
    | padTop n ls =
      let
        val space = spaces (rectWidth ls)
        fun pad 0 = nil
          | pad n = space :: pad (n - 1)
      in
        pad n @ ls
      end
  fun padBottom 0 ls = ls
    | padBottom n ls =
      let
        val space = spaces (rectWidth ls)
        fun pad 0 = nil
          | pad n = space :: pad (n - 1)
      in
        ls @ pad n
      end

  fun concatH rs =
      let
        fun loop [r] = r
          | loop (r :: rs) =
            map op@ (ListPair.zip (r, loop rs))
          | loop _ = nil
      in
        loop rs
      end
  val concatV = List.concat

  fun dim r = (rectWidth r, rectHeight r)
  fun resize (w, h) (fh, fv) r =
      let
        val (w', h') = dim r
        val (ws, hs) = (w - Int.min (w', w), h - Int.min (h', h))
        val r =
            case fh of
              FloatH.Left   => padRight ws r
            | FloatH.Right  => padLeft ws r
            | FloatH.Center => padLeft (ws div 2) (padRight ((ws + 1) div 2) r)
        val r =
            case fv of
              FloatV.Top    => padBottom hs r
            | FloatV.Bottom => padTop hs r
            | FloatV.Center => padTop (hs div 2) (padBottom ((hs + 1) div 2) r)
      in
        r
      end

  fun emptyRect (w, h) =
      let
        val l = spaces w
      in
        List.tabulate (h, fn _ => l)
      end

  fun width r =
      case r of
        Rect {contents, ...} => rectWidth contents
      | Row rs => (foldl op+ 0 o map width) rs
      | Col rs => (foldl Int.max 0 o map width) rs
      | Fill => 0
      | SpaceH w => w
      | SpaceV _ => 0
      | Indent {indentation, contents} =>
        indentation + width contents
      | Itemize {bullet, items} =>
        size bullet + 2 + (foldl Int.max 0 o map width) items
      | Enumerate {bullets = (_, w), items} =>
        w + (foldl Int.max 0 o map width) items
      | _ => die "toStringWithMaxWidth.width"

  fun height r =
      case r of
        Rect {contents, ...} => rectHeight contents
      | Row rs => (foldl Int.max 0 o map height) rs
      | Col rs => (foldl op+ 0 o map height) rs
      | Fill => 0
      | SpaceH _ => 0
      | SpaceV h => h
      | Indent {indentation, contents} =>
        height contents
      | Itemize {items, ...} =>
        (foldl (fn (h, a) => Int.max (1, h) + a) 0 o map height) items
      | Enumerate {items, ...} =>
        (foldl (fn (h, a) => Int.max (1, h) + a) 0 o map height) items
      | _ => die "toStringWithMaxWidth.height"

in
fun toStringWithMaxWidth maxw r =
    let
      fun loop (minw, minh) r =
          case r of
            Rect {floath, floatv, contents} =>
            (case contents of
               (* Hack: resize cant handle the empty rect, so we do it here explicitly *)
               nil => emptyRect (minw, minh)
             | _   => resize (minw, minh) (floath, floatv) contents
            )
          (* Look away! This code is ugly. But hey it works... Actually. *)
          | Row rs =>
            let
              val (space, spaces) =
                  foldl (fn (SpaceH w, (s, n)) => (s + w, n + 1)
                          | (SpaceV h, (s, n)) => (s, n + 1)
                          | (_, x)             => x
                        ) (0, 0) rs
              val fills = foldl (fn (Fill, a) => a + 1
                                  | (_, a)    => a) 0 rs
              val fillspace = Int.max (minw - width r, 0)
              val (filleach, fillexcess) =
                  if fills > 0 then
                    (fillspace div fills, fillspace mod fills)
                  else
                    (0, 0)
              val minh' = Int.max (height r, minh)
              val minw' = minw - space
              val n = length rs - spaces
            in
              (* This check is for stupid users who put spacing and
                 nothing else in a row *)
              if n > 0 then
                let
                  val minweach =
                      if fills > 0 then
                        0
                      else
                        minw' div n
                  val r = concatH (
                          map (fn SpaceH w => emptyRect (w, minh')
                                | SpaceV _ => emptyRect (0, minh')
                                | Fill     => emptyRect (filleach, minh')
                                | r        => loop (minweach, minh') r
                              ) rs)
                in
                  (* Probably should try to distribute the odd space *)
                  padRight (minw' mod n + fillexcess) r
                end
              else
                emptyRect (minw, minh)
            end
          | Col rs =>
            let
              val (space, spaces) =
                  foldl (fn (SpaceH w, (s, n)) => (s, n + 1)
                          | (SpaceV h, (s, n)) => (s + h, n + 1)
                          | (_, x)             => x
                        ) (0, 0) rs
              val fills = foldl (fn (Fill, a) => a + 1
                                  | (_, a)    => a) 0 rs
              val fillspace = Int.max (minh - height r, 0)
              val (filleach, fillexcess) =
                  if fills > 0 then
                    (fillspace div fills, fillspace mod fills)
                  else
                    (0, fillspace)
              val r = concatV (
                      map (fn SpaceH _ => emptyRect (minw, 0)
                            | SpaceV h => emptyRect (minw, h)
                            | Fill     => emptyRect (minw, filleach)
                            | r        => loop (minw, 0) r
                          ) rs)
            in
              (* Same thing with distributing the space as above *)
              padBottom fillexcess r
            end
          (* And now you can look again *)
          | Fill => emptyRect (minw, minh)
          | SpaceH _ => emptyRect (minw, minh)
          | SpaceV h => emptyRect (minw, h)
          | Indent {indentation, contents} =>
            padLeft indentation (loop (minw - indentation, minh) contents)
          | Itemize {bullet, items} =>
            let
              val bullet = " " ^ bullet ^ " "
              val bulletw = size bullet
              val bulletr = [explode bullet]
              val minw = minw - bulletw
            in
              concatV (
              map (fn r =>
                      let
                        val r = loop (minw, 1) r
                        val minh = rectHeight r
                      in
                        concatH [padBottom (minh - 1) bulletr,
                                 r]
                      end) items
              )
            end
          | Enumerate {bullets = (bs, w), items} =>
            concatV (
            map (fn (b, r) =>
                    let
                      val r = loop (minw - w, 1) r
                      val minh = rectHeight r
                    in
                      concatH [padBottom (minh - 1) [b],
                               r]
                    end) (ListPair.zip (bs, items))
            )
          | _ => die "toStringWithMaxWidth"

      fun strip l =
          let
            fun doit (#" " :: l) = doit l
              | doit l = l
          in
            (rev o doit o rev) l
          end
      fun toString [l] = implode (strip l)
        | toString (l :: ls) = implode (strip l) ^ "\n" ^ toString ls
        | toString _ = ""
    in
      toString (loop (maxw, 0) (toRects nil maxw r))
    end
end (* END toStringWithMaxWidth *)

val toString = toStringWithMaxWidth LINE_LENGTH

fun text' floath floatv format s =
    let
      fun text' s = Text {floath = floath,
                          floatv = floatv,
                          format = format,
                          contents = s}
    in
      case String.fields (fn c => #"\n" = c) s of
        [_] => text' s
      | ls  => Col (map text' ls)
    end

val text = text' FloatH.Left FloatV.Top Format.Plain
val paragraph = text' FloatH.Left FloatV.Top Format.Paragraph

fun indent r = Indent {indentation = BIG_INDENT,
                       contents = r}

fun ++ (Col rs, Col rs') = Col (rs @ rs')
  | ++ (r, Col rs) = Col (r :: rs)
  | ++ (Col rs, r) = Col (rs @ [r])
  | ++ (r, r') = Col [r, r']

local
  fun append rs rs' =
      case (rev rs, rs') of
        (Text {floath, floatv, format, contents = s} :: rs,
         Text {contents = s', ...} :: rs') =>
        rev rs @
        Text {floath = floath,
              floatv = floatv,
              format = format,
              contents = s ^ s'} ::
        rs'
      | _ => rs @ rs'
in
fun @@ (Col rs, Col rs') = Col (append rs rs')
  | @@ (Col rs, r') = Col (append rs [r'])
  | @@ (r, Col rs') = Col (append [r] rs')
  | @@ (r, r') =
    case append [r] [r'] of
      [r] => r
    | rs  => Col rs
end

fun || rss =
    let
      val strip = List.filter (fn x => x <> Fill)
    in
      Row ((case rss of
              (Row rs, Row rs') => strip rs @ strip rs'
            | (Row rs, r') => strip rs @ [r']
            | (r, Row rs') => r :: strip rs'
            | (r, r') => [r, r']
           ) @ [Fill])
    end

val nl = SpaceV 1

fun itemize rs = Itemize {bullet = NONE,
                          items = rs}

fun itemize' b rs = Itemize {bullet = SOME b,
                             items = rs}

fun enumerate' s rs = Enumerate {style = s,
                                 items = rs}
val enumerate = enumerate' EnumStyle.Number

local
  fun loop [r] = [r]
    | loop (r :: rs) = ++ (r, nl) :: loop rs
    | loop nil = nil
in
val itemizenl = itemize o loop
fun itemizenl' b = itemize' b o loop
val enumeratenl = enumerate o loop
fun enumeratenl' s = enumerate' s o loop
end

val row = Row
val column = Col

val print = fn r => print (toString r ^ "\n")
end
