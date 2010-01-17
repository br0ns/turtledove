(* Terrible implementation - an example of what not to do *)

functor MapTreeFn (Map : OrderedMap where type key = int) :> Tree =
struct
    fun die _ = raise Fail "MapTreeFn: Unimplemented."

    type node = Map.key
    (* data * parent * children *)
    type 'a context = 'a * node option * node list
    (* next_node * size * 'a context Map.t *)
    type 'a t = int * int * 'a context Map.t

    val root = 0

    fun create x = (1, 1, Map.singleton (root, (x, NONE, nil)))

    fun lookup (_, _, m) n =
        let
            val (x, _, _) = Map.lookup m n
        in
            x
        end

    fun insert (n', s, m) n x =
        let
            val m' = Map.modify (fn (x, p, cs) => (x, p, n' :: cs)) m n
            val m'' = Map.update m' (n', (x, SOME n, nil))
        in
            (n', (n' + 1, s + 1, m''))
        end

    fun insertTree (n', s, m) n (n'', s', m') =
        let
            val m = Map.modify (fn (x, p, cs) => (x, p, n' :: cs)) m n
            fun offset (x, pop, cs) =
                let
                    fun f n = n + n'
                in
                    (x, Option.map f pop, List.map f cs)
                end
            fun ins m nil = m
              | ins m ((n, t) :: ts) = ins (Map.update m (n + n', offset t)) ts
        in
            (n', (n' + n'', s + s', ins m (Map.toList m')))
        end

    fun insertTrees t n ts =
        foldl (fn (t', (ns, t)) =>
                  let
                      val (n, t) = insertTree t n t'
                  in
                      (n :: ns, t)
                  end
              ) (nil, t) ts
    val insertList = die

    val remove = die

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

    val sub = die

    fun modify f (n', s, m) n =
        (n', s, Map.modify (fn (x, p, cs) => (f x, p, cs)) m n)

    fun update (n', s, m) n x =
        (n', s, Map.modify (fn (_, p, cs) => (x, p, cs)) m n)

    fun toList (_, _, m) = map (fn (x, _, _) => x) (Map.range m)

    fun size (_, s, _) = s

    val height = die

    val map = die

    val fold = die

    structure Walk =
    struct
        fun this t = lookup t root
        val children = fn t => List.map (sub t) (children t root)
        fun go v ts =
            let
                val (_, t) = insertTrees (create v) root ts
            in
                t
            end
    end
end
