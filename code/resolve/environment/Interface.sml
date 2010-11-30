structure Interface :> Interface =
struct
datatype 'a int = I of {vi : 'a vi,
                        ti : ti,
                        si : 'a si}
withtype 'a vi = (Environment.status * 'a) Dictionary.t
     and ti = StringSet.t Dictionary.t
     and 'a si = 'a int Dictionary.t
     and 'a fi = ('a int * 'a int) Dictionary.t

type 'a t = 'a si * 'a fi
end
