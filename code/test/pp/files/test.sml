fun foo x = x

local
  open TextIO 
in
  val bar = 42
  and bar1 = bar
end

open String Int

fun baz [x] = x
  | baz (yss as y::ys) = (case yss of
                            y::ys => y
                          | _ => bar)
  | baz z = case z of
              z::zs => z
            | _ => bar1


val din = fn "mor" => "er saa fe' at naar hun gaar i biografen sider hun ved siden af alle."
           | "soster" => "er saa tyk at hun kan rulle uden at slaa hovedet."
           | "far" => "awsome"
           | x => x


fun curry_args foo bar baz = foo + bar * baz
