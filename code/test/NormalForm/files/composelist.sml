fun composelist [] b = b
  | composelist (x::xs) b = x (composelist xs b);
