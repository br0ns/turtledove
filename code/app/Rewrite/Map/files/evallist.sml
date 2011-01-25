fun evallist (f::fs, v) = f v :: evallist(fs, v) 
  | evallist ([], _) = []
