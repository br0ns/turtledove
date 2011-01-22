val here = Path.new (OS.FileSys.getDir ())
val st = SourceText.fromFile (Path.new' here "dummy.sml")
val ast = SMLParser.fromSourceText st

open SMLGrammar
val unwrap = Wrap.unwrap
val this = Tree.this
val children = Tree.children
val node = unwrap o this

fun find f t =
    let
      fun loop (t, ts) =
          foldl
            loop
            (if f (node t) then
               t :: ts
             else
               ts)
            (children t)
    in
      loop (t, nil)
    end

val funs = find (fn (Dec_Fun _) => true | _ => false)
val matches = children

val clauses = children
fun ident t =
    case (node t, map node ((children o hd o children) t)) of
      (FlatClause, Pat_Var i :: _) => i
    | _ => Crash.impossible "Foobar"

fun exp t = List.nth (children t, 2)

fun change old new (t, st) =
    let
      val ids = find (fn Pat_Var _ => true | Exp_Var _ => true | _ => false) t
      val ids = map
                  ((fn Pat_Var i => i
                     | Exp_Var i => i
                     | _ => Crash.impossible "foobar") o unwrap o this)
                  ids
      fun doit (i, st) =
          if Ident.toString (unwrap i) = old then
            SourceText.patch
              st
              (Wrap.left i)
              (Wrap.right i)
              (if Ident.fixity (unwrap i) = Fixity.Op then
                 "op " ^ new
               else
                 new)
          else
            st
    in
      foldl doit st ids
    end

val fs = (funs ast)
(* val _ = print (Int.toString (length fs)) *)

val ms = List.concat (map matches fs)
(* val _ = print (Int.toString (length ms)) *)
(* val _ = (print o Int.toString o length o children o hd) ms *)

fun correct (m, st) =
    let
      val cs = clauses m
      val names = map (Ident.toString o unwrap o ident) cs
    in
      case names of
        new :: old :: _ => if old = new then
                             st
                           else
                             foldl (change old new) st cs
      | _ => st
    end

val st = foldl correct st ms

(* val st = SourceText.patch st 32 35 "fact" *)
(* val st = SourceText.patch st 20 23 "fact" *)

(* val _ = print (SourceText.toString st) *)

val _ = SourceText.write st
