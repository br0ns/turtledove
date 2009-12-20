

functor MapTreeFn (Map : OrderedMap where type key = int) :> Tree =
struct
    type node = Map.key
    (* data * parent * children *)
    type 'a context = 'a * node option * node list
    (* next_node * size * 'a context Map.t *)
    type 'a t = int * int * 'a context Map.t

    val root = 0

    fun create x = (1, 1, Map.singleton (root, (x, NONE, nil)))

    fun insert (n', s, m) n x =
        let
            val m' = Map.modify (fn (x, p, cs) => (x, p, n' :: cs)) m n
            val m'' = Map.update m' (n', (x, SOME n, nil))
        in
            (n', (n' + 1, s + 1, m''))
        end

    fun insertTree _ = raise Fail ""

    fun delete (n', s, m) n =
        let
            val (_, pOp, cs) = Map.lookup m n
            val p = valOf pOp
            val m' = Map.modify (fn (x, p, cs) =>
                                    (x, p, List.filter (fn n' => n' <> n) cs)
                                ) m p

            fun delete' (n, (m, d)) =
                let
                    val (_, _, cs) = Map.lookup m n
                in
                    foldl delete' (Map.delete m n, d + 1) cs
                end
            val (m'', d) = delete' (n, (m', 0))
        in
            (n', s - d, m'')
        end

    fun value (_, _, m) n =
        let
            val (x, _, _) = Map.lookup m n
        in
            x
        end

    fun children (_, _, m) n =
        let
            val (_, _, cs) = Map.lookup m n
        in
            cs
        end

    fun parent (_, _, m) n =
        let
            val (_, p, _) = Map.lookup m n
        in
            p
        end

    fun sub _ _ = raise Fail ""

    fun modify f (n', s, m) n =
        (n', s, Map.modify (fn (x, p, cs) => (f x, p, cs)) m n)

    fun update (n', s, m) n x =
        (n', s, Map.modify (fn (_, p, cs) => (x, p, cs)) m n)

    fun toList _ = raise Fail ""

    fun size (_, s, _) = s

    fun height _ = raise Fail ""

    fun map _ = raise Fail ""
    fun foldpr _ = raise Fail ""
    fun foldin _ = raise Fail ""
    fun foldpo _ = raise Fail ""
                         
end
