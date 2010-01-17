structure Ident :> Ident =
struct
    fun id x = x

    type id = string
    val idToString = id
    fun showId id = "Id (" ^ id ^ ")"

    type longid = string list * string
    fun longIdToString (qual, id) =
        foldr (fn (x, a) => x ^ "." ^ a) "" qual ^ id
    fun showLongId lid = "LongId (" ^ longIdToString lid ^ ")"

    val mkId : id
    fun inventId () = "<uniq." ^ UniqId.next () ^ ">"

    fun idToLongId id = (nil, id)

    fun mkLongId strs =
        case (rev strs) of
	        nil     => Crash.impossible "Ident.mkLongId"
	      | x :: xs => (rev xs, x)

    val inventLongId = idToLongId o inventId

    val implodeLongId = id
    val explodeLongId = id

    fun unqualified (nil, _) = true
      | unqualified _ = false

    fun longIdToId (nil, id) = id
      | longIdToId _ = Crash.impossible "Ident.longIdToId"

    val bogus = "<bogus>"
end
