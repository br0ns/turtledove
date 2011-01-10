fun foo x = x

open TextIO 

val bar = 42
and bar1 = bar

open String Int

fun baz [x] = x
  | baz y = (case y of
               y::ys => y
             | _ => 42)
  | baz z = case z of
              z::zs => z
            | _ => bar

val din = fn mor => "er saa fe' at naar hun gaar i biografen sider hun ved siden af alle."
           | soster => "er saa tyk at hun kan rulle uden at slaa hovedet."
           | far => "awsome"


