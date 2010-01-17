functor TrieOrderedSetFn (Map : OrderedMap) :> OrderedSet where type element = Map.key list =
struct
fun die _ = raise Fail "TrieTreeFn: Unimplemented"

type element = Map.key list
datatype t = T of bool * t Map.t

val empty = T (false, Map.empty)

fun insert (T (_, m)) nil = T (true, m)
  | insert (T (b, m)) (k :: ks) =
    T (b, Map.update m (k, insert (
                           case Map.lookup m k of
                             SOME t => t
                           | NONE   => empty) ks
                       )
      )

val fromList = List.foldl (fn (x, s) => insert s x) empty

fun toList (T (b, m)) =
    let
      val es = List.concat (
               List.map (fn (k, t) =>
                            List.map (fn e => k :: e) (toList t)
                        ) (Map.toList m)
               )
    in
      if b then
        nil :: es
      else
        es
    end

val singleton = die
val delete = die
val union = die
val inter = die
val diff = die
val subset = die
val equal = die
val member = die
val isEmpty = die
val compare = die
val card = die
val partition = die
val filter = die
val exists = die
val all = die
val find = die
val app = die
val map = die
val mapPartial = die
val fold = die
val foldl = die
val foldr = die
val split = die
val splitLeast = die
val splitGreatest = die
val least = die
val greatest = die
val some = die
val toString = die
end
