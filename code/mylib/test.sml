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
;Benchmark.pause ();
;Benchmark.print ();
(* val _ = (print o Show.list Show.int o Tree.toList) t *)

val i = ref 0
fun loop 0 = nil
  | loop n =
    [Tree.Walk.go (i := !i + 1 ; !i) (loop (n - 1))
   , Tree.Walk.go (i := !i + 1 ; !i) (loop (n - 1))
   , Tree.Walk.go (i := !i + 1 ; !i) (loop (n - 1))
   , Tree.Walk.go (i := !i + 1 ; !i) (loop (n - 1))
   , Tree.Walk.go (i := !i + 1 ; !i) (loop (n - 1))
    ]

;Benchmark.start ();
val t = Tree.Walk.go 0 (loop 6)
;Benchmark.pause ();
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

;Benchmark.start ();
val t = loop 5 (Tree.create 0) [Tree.root]
;Benchmark.stop ();
;Benchmark.print ();

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
