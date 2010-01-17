(* Fully implemented - Yay *)

functor PlainTreeFn (Map : OrderedMap where type key = int) :> Tree =
struct
    type node = int list
    exception Node

    structure Node =
    struct
    type t = node
    fun toString [n] = Int.toString n
      | toString (n :: ns) = Int.toString n ^ "," ^ toString ns
      | toString _ = ""
    fun fromString s = map (valOf o Int.fromString)
                           (String.tokens (fn c => c = #",") s)
        handle Option.Option => raise Node
    end

    (* data * next_node * children *)
    datatype 'a t = T of 'a * int * 'a t Map.t

    val root = nil

    fun create x = T (x, 0, Map.empty)

    fun insertTrees t ns ts' =
        let
            (* Worst hack ever *)
            val n = ref 0
            val s = length ts'

            fun ins (n :: ns) (T (v, n', ts)) =
                T (v, n', Map.modify (ins ns) ts n)
              | ins nil (T (v, n', ts)) =
                (n := n' ;
                 T (v, n' + s,
                    #1 (foldl (fn (t', (ts, n')) =>
                                  (Map.update ts (n', t'), n' + 1)
                              ) (ts, n') ts')
                   )
                )
        in
            (List.tabulate (s, fn x => ns @ [!n + x]), ins ns t)
        end

    fun insertList t n xs = insertTrees t n (map create xs)
    fun insertTree t n t' =
        let
            val (ns, t) = insertTrees t n [t']
        in
            (hd ns, t)
        end
    fun insert t n = insertTree t n o create

    fun remove t ns =
        let
            val t' = ref t
            fun remove' (T (v, n', ts)) [n] =
                let
                    val (t, ts') = Map.remove ts n
                in
                    (t' := t ;
                     T (v, n', ts')
                    )
                end
              | remove' (T (v, n', ts)) (n :: ns) =
                T (v, n', Map.modify (fn t => remove' t ns) ts n)
              | remove' _ _ = raise Domain
        in
            (!t', remove' t ns)
        end

    fun delete (T (v, n', ts)) [n] =
        T (v, n', Map.delete ts n)
      | delete (T (v, n', ts)) (n :: ns) =
        T (v, n', Map.modify (fn t => delete t ns) ts n)
      | delete _ _ = raise Domain

    fun lookup (T (v, _, _)) nil = v
      | lookup (T (_, _, ts)) (n :: ns) =
        case Map.lookup ts n of
          NONE   => raise Node
        | SOME t => lookup t ns

    fun children t ns =
        let
            fun children' (T (_, _, ts)) nil =
                map (fn n => ns @ [n]) (Map.domain ts)
              | children' (T (_, _, ts)) (n :: ns) =
                case Map.lookup ts n of
                  NONE   => raise Node
                | SOME t => children' t ns
        in
            children' t ns
        end

    fun parent _ nil = NONE
      | parent _ [_] = SOME nil
      | parent t (n :: ns) = SOME (n :: valOf (parent t ns))

    fun sub t nil = t
      | sub (T (_, _, ts)) (n :: ns) =
        case Map.lookup ts n of
          SOME t => sub t ns
        | NONE => raise Node

    fun modify f (T (v, n', ts)) nil = T (f v, n', ts)
      | modify f (T (v, n', ts)) (n :: ns) =
        T (v, n', Map.modify (fn t => modify f t ns) ts n)

    fun update (T (_, n', ts)) nil v' = T (v', n', ts)
      | update (T (v, n', ts)) (n :: ns) v' =
        T (v, n', Map.modify (fn t => update t ns v') ts n)

    fun toList (T (v, _, ts)) = v :: (List.concat o map toList o Map.range) ts

    fun size (T (_, _, ts)) = 1 + (foldl op+ 0 o map size o Map.range) ts

    fun height (T (_, _, ts)) = 1 + (foldl Int.max 0 o map height o Map.range) ts

    fun map f (T (v, n', ts)) = T (f v, n', Map.map (map f) ts)

    fun fold f b (T (v, _, ts)) =
        foldl (fn (t, a) => fold f a t) (f (v, b)) (Map.range ts)

    structure Walk =
    struct
        type 'a tree = 'a t
        type 'a t = 'a tree * 'a tree list
        fun init t = (t, nil)
        fun this (T (v, _, _), _) = v
        fun children (p as T (_, _, ts), w) = List.map (fn t => (t, p :: w)) (Map.range ts)
        fun parent (_, p :: w) = SOME (p, w)
          | parent _ = NONE
    end

    fun join v ts =
        let
          val (_, t) = insertTrees (create v) root ts
        in
          t
        end

    fun toString pr t = "[Tree with " ^ Int.toString (size t) ^ " nodes]"
end
