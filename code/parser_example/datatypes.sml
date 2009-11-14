signature DATATYPES =
sig
    datatype A    = A of Pat * Proc
         and Pi   = Pi of Proc list
         and Pat  = Pat of Path
         and Path = Name of string * string * int * int
         and Proc = New of Path * Proc
                  | Output of Path * V
                  | Input of Path * A
                  | Parallel of Proc list
         and V    = V of Path
end

structure DataTypes =
struct
    datatype A    = A of Pat * Proc
         and Pi   = Pi of Proc list
         and Pat  = Pat of Path
         and Path = Name of string * string * int * int
         and Proc = New of Path * Proc
                  | Output of Path * V
                  | Input of Path * A
                  | Parallel of Proc list
         and V    = V of Path
end
