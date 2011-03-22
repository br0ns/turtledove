fun foo (x :: y :: ys) = x + 42 :: foo (y :: ys)
  | foo (x :: nil) = x + 42 :: foo nil
  | foo _ = nil
