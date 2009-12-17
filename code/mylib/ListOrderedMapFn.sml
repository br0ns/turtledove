

functor ListOrderedMapFn (Key : Ordered) :> OrderedMap where type key = Key.t =
struct

    type key = Key.t
    type 'a t = (key * 'a) list

    val compare = Key.compare

    val empty = nil

    fun singleton x = [x]

    fun insert nil x = SOME [x]
      | insert ((k', v') :: ys) (k, v) =
        case compare k k' of
            GREATER =>
            (case insert ys (k, v) of
                 SOME ys => SOME ((k', v') :: ys)
               | NONE    => NONE
            )
          | EQUAL   => NONE
          | LESS    => SOME ((k, v) :: (k', v') :: ys)
                       
    fun delete nil _ = nil
      | delete ((k', v') :: ys) k =
        case compare k k' of
            GREATER => (k', v') :: delete ys k
          | _       => ys

    fun update nil x = [x]
      | update ((k', v') :: ys) (k, v) =
        case compare k k' of
            GREATER => (k', v') :: update ys (k, v)
          | EQUAL   => (k, v) :: ys
          | LESS    => (k, v) :: (k', v') :: ys

    fun change nil _ _ = NONE
      | change ((k', v') :: ys) k f =
        case compare k k' of
            GREATER =>
            (case change ys k f of
                 SOME ys => SOME ((k', v') :: ys)
               | NONE    => NONE
            )
          | EQUAL   => SOME ((k', f v') :: ys)
          | LESS    => NONE

    fun updateList ys xs = foldl (fn (x, ys) => update ys x) ys xs

    fun fromList xr = updateList empty xr

    fun lookup nil _ = NONE
      | lookup ((k', v') :: ys) k =
        case compare k k' of
            GREATER => lookup ys k
          | EQUAL   => SOME v'
          | LESS    => NONE

    fun inDomain nil _ = false
      | inDomain ((k', _) :: ys) k =
        case compare k k' of
            GREATER => inDomain ys k
          | EQUAL   => true
          | LESS    => false

    val isEmpty = null

    val size = List.length

    fun toList ys = ys

    fun domain ys = map (fn (k, _) => k) ys

    fun range ys = map (fn (_, v) => v) ys

    fun first nil = raise Empty
      | first ((_, v) :: _) = v

    fun firsti nil = raise Empty
      | firsti (y :: _) = y

    fun last nil = raise Empty
      | last ys = ((fn (_, v) => v) o hd o rev) ys

    fun lasti nil = raise Empty
      | lasti ys = hd (rev ys)

    fun splitFirst nil = NONE
      | splitFirst (y :: ys) = SOME (y, ys)

    fun splitLast ys =
        case splitFirst (rev ys) of
            NONE         => NONE
          | SOME (y, ys) => SOME (y, rev ys)

    val split = splitFirst

    fun unimp () = raise Fail "Not implemented"

    fun collate _ = unimp ()

    fun partition _ = unimp ()
    fun partitioni _ = unimp ()
    fun filter _ = unimp ()
    fun filteri _ = unimp ()
    fun remove _ = unimp ()
    fun removei _ = unimp ()
    fun exists _ = unimp ()
    fun existsi _ = unimp ()
    fun all _ = unimp ()
    fun alli _ = unimp ()
    fun find _ = unimp ()
    fun findi _ = unimp ()

    fun app _ = unimp ()
    fun appi _ = unimp ()
    fun map _ = unimp ()
    fun mapi _ = unimp ()
    fun mapPartial _ = unimp ()
    fun mapPartiali _ = unimp ()
    fun foldl _ = unimp ()
    fun foldli _ = unimp ()
    fun foldr _ = unimp ()
    fun foldri _ = unimp ()

    fun union _ = unimp ()
    fun unioni _ = unimp ()
    fun inter _ = unimp ()
    fun interi _ = unimp ()

    fun merge _ = unimp ()
    fun mergi _ = unimp ()

    fun plus _ = unimp ()
                 
    fun toString _ = unimp ()
end
