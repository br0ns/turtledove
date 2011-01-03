functor InfixStack (
type ident = Grammar.ident
type ast = (ident, ident, int) Grammar.ast
val pair : ast -> ast -> ast
val asId : ast -> ident option
val toTree : ident -> ast
val apply : ast -> ast -> ast
) =
struct

val @@ = uncurry apply
infix @@
fun die s = Crash.impossible ("Infixing.InfixStack: " ^ s)

val fixity = Ident.fixity o Wrap.unwrap
open Fixity

exception TooFewArgs of ident
      and Assoc of ident * ident

datatype entry = Ident of ident
               | App

fun apply (entry, snd :: fst :: rest) =
    let
      val thepair = pair fst snd
    in
      (case entry of
         Ident id => (toTree id) @@ thepair
       | App => fst @@ snd
      ) :: rest
    end
  | apply (entry, _) =
    case entry of
      Ident id => raise TooFewArgs id
    | App      => die "apply"

fun assocLeft (App, _) = true
  | assocLeft (_, App) = false
  | assocLeft (a, b) =
    prec a > prec b orelse
    prec a = prec b andalso bothLeft (a, b)
and prec (Ident id) =
    (case case fixity id of
            InfixL n => n
          | InfixR n => n
          | _ => die "prec" of
       SOME n => n
     | NONE => 0) (* Default infix precedence *)
  | prec _ = die "prec"
and bothLeft (Ident a, Ident b) =
    (case (fixity a, fixity b) of
       (InfixL _, InfixL _) => true
     | (InfixR _, InfixR _) => false
     | _ => raise Assoc (a, b)
    )
  | bothLeft _ = die "bothLeft"

fun flushHigher (entry, stack, output) =
    case stack of
      nil => (nil, output)
    | top :: rest =>
      if assocLeft (top, entry) then
        flushHigher (entry, rest, apply (top, output))
      else
        (stack, output)

fun flushAll (nil, [x]) = x
  | flushAll (top :: rest, output) = flushAll (rest, apply (top, output))
  | flushAll _ = die "flushAll"

fun process (input, stack, arg, output) =
    case input of
      nil => flushAll (stack, output)
    | this :: rest =>
      case case asId this of
           SOME id =>
           (case fixity id of
              InfixL _ => SOME id
            | InfixR _ => SOME id
            | _ => NONE)
         | NONE => NONE of
        SOME id => operator (Ident id, rest, stack, output)
      | NONE => if arg then
                  operator (App, input, stack, output)
                else
                  process (rest, stack, true, this :: output)

and operator (entry, input, stack, output) =
    let
      val (stack', output') = flushHigher (entry, stack, output)
    in
      process (input, entry :: stack', false, output')
    end

fun resolve trees =
    process (trees, nil, false, nil)
end
