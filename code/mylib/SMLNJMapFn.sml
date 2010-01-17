functor SMLNJMapFn (Key : Ordered) :> OrderedMap where type key = Key.t =
struct
    fun die _ = raise Fail "SMLNJMapFn: Unimplemented"

    structure Map = RedBlackMapFn
                        (struct
                         type ord_key = Key.t
                         fun compare (x, y) = Key.compare x y
                         end)
    type key = Key.t
    type 'a t = 'a Map.map

    val empty = Map.empty
    fun singleton (k, v) = Map.insert (empty, k, v)
    val insert = die
    val fromList = die
    fun update m (k, v) = Map.insert (m, k, v)
    fun remove m k =
        let
            val (m', v) = Map.remove (m, k)
        in
            (v, m')
        end
    fun delete m k =
        let
            val (m', _) = Map.remove (m, k)
        in
            m'
        end
    fun modify f m k =
        let
            val (m', v) = Map.remove (m, k)
        in
            Map.insert (m', k, f v)
        end
    fun lookup m k = Map.find (m, k)
    val inDomain = die
    val isEmpty = die
    val size = die
    val toList = Map.listItemsi
    fun domain m = map (fn (k, _) => k) (toList m)
    fun range m = map (fn (_, v) => v) (toList m)
    val first = die
    val firsti = die
    val last = die
    val lasti = die
    val split = die
    val splitFirst = die
    val splitLast = die
    val collate = die
    val partition = die
    val partitioni = die
    val filter = die
    val filteri = die
    val exists = die
    val existsi = die
    val all = die
    val alli = die
    val find = die
    val findi = die
    val app = die
    val appi = die
    val map = Map.map
    val mapi = die
    val mapPartial = die
    val mapPartiali = die
    val foldl = die
    val foldli = die
    val foldr = die
    val foldri = die
    val union = die
    val unioni = die
    val inter = die
    val interi = die
    val merge = die
    val mergi = die
    val plus = die
    val toString = die
end
