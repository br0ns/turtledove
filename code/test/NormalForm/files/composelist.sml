fun composelist [] x = x 
  | composelist (f :: []) x = f(x) 
  | composelist (f :: fs) x = f(composelist fs x)


fun composelist [] x = x 
  | composelist (y::ys) x = y (composelist (ys) x)
