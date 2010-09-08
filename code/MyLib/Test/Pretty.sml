datatype tree = T of int * tree * tree
              | E

open Layout
infix ^^ ++ \ & \\ &&

fun pp (T (x, l, r)) =
    let
      val body = punctuate comma [int x, pp l, pp r]
    in
      txt "T" ++ choice (parens (hsep body),
                         lparen ^^ align (vsep body) \\ rparen
                        )
    end
  | pp E = txt "E"

fun gen 0 = E
  | gen n = T (n, gen (n - 1), gen (n - 1))

(* ;Benchmark.start (); *)
(* ;println (SOME 80) (pp (gen 16)); *)
(* ;Benchmark.stopAndPrint ""; *)

; println (SOME 80) (str (TextIO.inputAll TextIO.stdIn)) ;
; TextIO.println (String.wordwrap 20 "this is a test more thatn twenty chars wide\nwith a newline thrown in");
