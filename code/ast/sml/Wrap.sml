structure Wrap :> Wrap =
struct
type ('a, 'b) t = {node: 'a, data: 'b}

fun wrap n d = {node = n, data = d}
fun unwrap ({node, ...} : ('a, 'b) t) = node
fun data ({data, ...} : ('a, 'b) t) = data
fun modify f {node, data} = {node = f node, data = data}
end
