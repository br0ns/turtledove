val print = fn s => print (s ^ "\n")
val i = ref 1

fun insertfive t n =
    let
        val (n0, t) = Tree.insert t n (!i)
        val (n1, t) = Tree.insert t n (!i + 1)
        val (n2, t) = Tree.insert t n (!i + 2)
        val (n3, t) = Tree.insert t n (!i + 3)
        val (n4, t) = Tree.insert t n (!i + 4)
    in
        i := !i + 5 ;
        ([n0, n1, n2, n3, n4], t)
    end

fun loop 0 t _ = t
  | loop n t ns =
    let
        val (ns', t') =
            foldl (fn (n, (ns, t)) =>
                      let
                          val (ns', t') = insertfive t n
                      in
                          (ns @ ns', t')
                      end
                  ) (nil, t) ns
    in
        loop (n - 1) t' ns'
    end
;Benchmark.start ();
val t = loop 5 (Tree.create 0) [Tree.root]
;Benchmark.stop ();
;Benchmark.print ();
(* val _ = (print o Show.list Show.int o Tree.toList) t *)

(* structure Tree = TrieTreeFn ( *)
(*                  (\* ListOrderedMapFn *\) *)
(*                  SMLNJMapFn *)
(*                      (struct *)
(*                       type t = int *)
(*                       fun compare x y = Int.compare (x, y) *)
(*                       end) *)
(*                  ) *)

val i = ref 0
fun loop 0 = nil
  | loop n =
    [Tree.join (i := !i + 1 ; !i) (loop (n - 1))
   , Tree.join (i := !i + 1 ; !i) (loop (n - 1))
   , Tree.join (i := !i + 1 ; !i) (loop (n - 1))
   , Tree.join (i := !i + 1 ; !i) (loop (n - 1))
   , Tree.join (i := !i + 1 ; !i) (loop (n - 1))
    ]

;Benchmark.start ();
val t = Tree.join 0 (loop 8)
;Benchmark.stop ();
;Benchmark.print ();

val i = ref 1

fun insertfive t n =
    let
        val (n0, t) = Tree.insert t n (!i)
        val (n1, t) = Tree.insert t n (!i + 1)
        val (n2, t) = Tree.insert t n (!i + 2)
        val (n3, t) = Tree.insert t n (!i + 3)
        val (n4, t) = Tree.insert t n (!i + 4)
    in
        i := !i + 5 ;
        ([n0, n1, n2, n3, n4], t)
    end

fun loop 0 t _ = t
  | loop n t ns =
    let
        val (ns', t') =
            foldl (fn (n, (ns, t)) =>
                      let
                          val (ns', t') = insertfive t n
                      in
                          (ns @ ns', t')
                      end
                  ) (nil, t) ns
    in
        loop (n - 1) t' ns'
    end

(* ;Benchmark.start (); *)
(* val t = loop 6 (Tree.create 0) [Tree.root] *)
(* ;Benchmark.stop (); *)
(* ;Benchmark.print (); *)

(* local *)
(*     structure Walk = *)
(*     struct *)
(*         local *)
(*             open Tree *)
(*         in *)
(*         fun this t = lookup t root *)
(*         fun children t = List.map (sub t) (Tree.children t root) *)
(*         fun go v ts = *)
(*             let *)
(*                 val (_, t) = insertTrees (create v) root ts *)
(*             in *)
(*                 t *)
(*             end *)
(*         end *)
(*     end *)
(*     open Walk *)
(* in *)
(* fun inc2 t = go (this t + 2) (map inc2 (children t)) *)
(* end *)

(* val t = loop 3 (Tree.create 0) [Tree.root] *)
(* val _ = (print o Show.list Show.int o Tree.toList) t *)
(* val t = inc2 t *)
(* val _ = (print o Show.list Show.int o Tree.toList) t *)


fun split nil = (nil, nil)
  | split [x] = ([x], nil)
  | split (x :: y :: zs) =
    let
        val (xs, ys) = split zs
    in
        (x :: xs, y :: ys)
    end

fun build nil = nil
  | build (z :: zs) =
    let
        open Tree
        val (xs, ys) = split zs
    in
        [join z (build xs @ build ys)]
    end

val t = hd (build (List.tabulate (1000, fn x => x)))

fun leftmost t =
    let
        open Tree.Walk
        fun find w =
            case children w of
                nil    => this w
              | l :: _ => find l
    in
        find (init t)
    end

val _ = print (Show.int (leftmost t))

fun walk t =
    let
        open Tree.Walk
        fun loop w =
            let
                val _ = print (Show.int (this w))
                val i = (hd o explode o valOf o TextIO.inputLine) TextIO.stdIn
            in
                case i of
                    #"w" =>
                    (case parent w of
                         SOME p => loop p
                       | NONE   => (print "No parent node." ;
                                    loop w)
                    )
                  | #"a" =>
                    (case children w of
                         l :: _ => loop l
                       | _      => (print "No left child." ;
                                    loop w)
                    )
                  | #"d" =>
                    (case children w of
                         _ :: r :: _ => loop r
                       | _           => (print "No right child." ;
                                         loop w)
                    )
                  | #"q" => ()
                  | _    => (print "Press 'q', 'w', 'a' or 'd'." ;
                             loop w)
            end
    in
        loop (init t)
    end

val _ = walk t

(*
           0
     1           2
  3     5     4     6
 7 11  9 13  8 12 10 14


In general the sequence x1...xn will get you to x1 + 2(x2 + 2(...2(xn)...))
where left = 1 and right = 2

so llrrlr = 1 + 2(1 + 2(2 + 2(2 + 2(1 + 2(2))))) = 107
and 999 = 1 + 2(1 + 2(1 + 2(2 + 2(1 + 2(2 + 2(2 + 2(2 + 2(2)))))))) = lllrlrrrr
*)
