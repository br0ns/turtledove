

functor MapTreeFn (Map : OrderedMap where type key = int) :> Tree =
struct

    type node = Map.key
    type 'a t = int * int * ('a * node option * node list) Map.t

    val root = 0

    fun create x = (1, 1, Map.singleton (root, (x, NONE, nil)))

    fun insert (n', s, m) n x =
        let
            val m' = valOf (Map.change m n (fn (x, p, cs) => (x, p, n' :: cs)))
            val m'' = valOf (Map.insert m' (n', (x, SOME n, nil)))
        in
            (n', (n' + 1, s + 1, m''))
        end

    fun insertList _ = raise Fail ""
    fun insertTree _ = raise Fail ""

    fun delete _ = raise Fail ""

    fun context (_, _, m) n =
        case Map.lookup m n of
            SOME c => c
          | NONE   => raise Fail "Impossible: Invalid node"

    fun value t n = (fn (x, _, _) => x) (context t n)

    fun children t n = (fn (_, _, cs) => cs) (context t n)

    fun parent t n = (fn (_, p, _) => p) (context t n)

    fun change _ = raise Fail ""
    fun update _ = raise Fail ""

    fun size (_, s, _) = s

    fun height _ = raise Fail ""

    fun map _ = raise Fail ""
    fun foldpr _ = raise Fail ""
    fun foldin _ = raise Fail ""
    fun foldpo _ = raise Fail ""
                         
end
