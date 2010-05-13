structure Wrap :> Wrap =
struct
type 'a t = {node: 'a, left: int, right: int}

fun wrap n l r = {node = n, left = l, right = r}
fun unwrap ({node, ...} : 'a t) = node
fun left ({left, ...} : 'a t) = left
fun right ({right, ...} : 'a t) = right

fun modify f {node, left, right} = {node = f node, left = left, right = right}
end
