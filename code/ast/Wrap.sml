structure Wrap :> Wrap =
struct
type ('a, 'b) t = {node: 'a, left: 'b, right: 'b}

fun wrap n l r = {node = n, left = l, right = r}
fun unwrap ({node, ...} : ('a, 'b) t) = node
fun left ({left, ...} : ('a, 'b) t) = left
fun right ({right, ...} : ('a, 'b) t) = right
fun modify f {node, left, right} =
    {node = f node, left = left, right = right}
fun extend f {node, left, right} l r =
    {node = node, left = f left l, right = f right r}
end
