

functor RedBlackOrderedSetFn (Element : Ordered) :> OrderedSet where type element = Element.t =
struct
    type element = Element.t

    datatype color = R | B
    datatype t = E
               | T of color * t * element * t

    fun die _ = raise Fail "Unimplemented"

    val empty = E

    fun singleton x = T (B, E, x, E)

    fun balance (B, T (R, T (R, a, x, b), y, c), z, d) =
        T (R, T (B, a, x, b), y, T (B, c, z, d))
      | balance (B, T (R, a, x, T (R, b, y, c)), z, d) = 
        T (R, T (B, a, x, b), y, T (B, c, z, d))
      | balance (B, a, x, T (R, T (R, b, y, c), z, d)) = 
        T (R, T (B, a, x, b), y, T (B, c, z, d))
      | balance (B, a, x, T (R, b, y, T (R, c, z, d))) = 
        T (R, T (B, a, x, b), y, T (B, c, z, d))
      | balance x = T x

    fun insert s x =
        let
            fun insert' E = T (R, E, x, E)
              | insert' (s as T (c, l, y, r)) =
                case Element.compare x y of
                    LESS    => balance (c, insert' l, y, r)
                  | GREATER => balance (c, l, y, insert' r)
                  | EQUAL   => s
            val T (_, l, x, r) = insert' s
        in
            T (B, l, x, r)
        end

    fun member E _ = false
      | member (T (_, l, y, r)) x =
        case Element.compare x y of
            LESS    => member l x
          | GREATER => member r x
          | EQUAL   => true

    val delete = die

    val fromList = foldl (fn (x, s) => insert s x) empty

    fun foldl _ b E = b
      | foldl f b (T (_, l, x, r)) = foldl f (f (x, foldl f b l)) r

    fun foldr _ b E = b
      | foldr f b (T (_, l, x, r)) = foldr f (f (x, foldr f b r)) l

    fun partition p = foldl (fn (x, (s, s')) =>
                                if p x then
                                    (insert s x, s')
                                else 
                                    (s, insert s' x)
                            ) (empty, empty)
    fun filter p = foldl (fn (x, s) => if p x then insert s x else s) empty

    fun exists _ E = false
      | exists p (T (_, l, x, r)) = p x orelse exists p l orelse exists p r

    fun all _ E = true
      | all p (T (_, l, x, r)) = p x andalso all p l andalso all p r

    fun find _ E = NONE
      | find p (T (_, l, x, r)) =
        if p x then
            SOME x
        else
            case find p l of
                SOME x => SOME x
              | NONE => find p r

    val union = foldl (fn (e, s) => insert s e)
    fun inter s = filter (member s)
    val diff = foldl delete

    fun subset s = all (member s)

    fun isEmpty E = true
      | isEmpty _ = false

    val compare = die

    fun toList E = nil
      | toList (T (_, l, x, r)) = toList l @ x :: toList r

    fun equal s s' = toList s = toList s'

    fun card E = 0
      | card (T (_, l, _, r)) = 1 + card l + card r

    fun app _ E = ()
      | app f (T (_, l, x, r)) = (app f l ; f x ; app f r)

    val map = die
    val mapPartial = die

    val split = die
    val splitLeast = die
    val splitGreatest = die

    fun least E = raise Empty
      | least (T (_, E, x, _)) = x
      | least (T (_, l, _, _)) = least l

    fun greatest E = raise Empty
      | greatest (T (_, _, x, E)) = x
      | greatest (T (_, _, _, r)) = greatest r

    fun some E = raise Empty
      | some (T (_, _, x, _)) = x

    val toString = die
end
