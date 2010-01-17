functor FlagsFn (S : Set) : Flags =
struct
    open S
    val flags = ref empty

    fun set flag =
        flags := insert (!flags) flag

    fun unset flag =
        flags := delete (!flags) flag

    fun get flag =
        member (!flags) flag
end
