fun member (_, nil) = false
  | member (x, y :: ys) = x = y orelse member (x, ys)

fun sublist (nil, _) = true
  | sublist (x :: xs, ys) = member (x, ys) andalso sublist (xs, ys)
