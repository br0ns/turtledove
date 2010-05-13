val id = General.id
val ^* = General.^*
val $ = General.$
val curry = General.curry
val uncurry = General.uncurry
val pair = General.pair
val curry3 = General.curry3
val uncurry3 = General.uncurry3
val triple = General.triple
val curry4 = General.curry4
val uncurry4 = General.uncurry4
val quadruple = General.quadruple
val to = General.to
val inc = General.inc
val dec = General.dec
infix ^* $ to

val println = TextIO.println
val ltoi = Int.fromLarge
val itol = Int.toLarge

datatype either = datatype Either.either
exception Either = Either.Either
val ofLeft = Either.ofLeft
val ofRight = Either.ofRight
val either = Either.either

val fst = Pair.fst
val snd = Pair.snd
