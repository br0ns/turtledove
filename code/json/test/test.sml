val print = fn s => print (s ^ "\n")

val cnumberlist = JSON.Converter.array JSON.Converter.number
val json = JSON.to cnumberlist (map real [1,2,3])
val _ = print json
val json = JSON.read "{\"bleh\" : {     }}"
val json = JSON.write json
val _ = print json

datatype sn = String of string
            | Int of int
val numberorstring =
    JSON.Converter.make
    {toJSON   = fn String s => JSON.String s
                 | Int n    => JSON.Number (real n)
   , fromJSON = fn JSON.String s => String s
                 | JSON.Number r => Int (round r)
    }

fun showsn (String s) = s
  | showsn (Int n) = Int.toString n

val snlist = JSON.from (JSON.Converter.array numberorstring) "[\"hep\", 7, \"hey\", 42]"
val _ = print (Show.list showsn snlist)
