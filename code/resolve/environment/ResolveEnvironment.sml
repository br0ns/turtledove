structure ResolveInvironment =
struct

fun die _ = Crash.impossible "ResolveInvironment"

fun run t =
    let
      fun node t = Wrap.unwrap $ Tree.this t
      fun children t = Tree.children t
      fun join t e ts = 
      fun next e t

      fun loop e t =
          case node t of
            Topdecs => next e t
          | Strdecs => next e t
          | Strdec_Str => next e t
          | Strdec_Local =>
            (case children t of
               [sdecs1, sdecs2] =>
               let
                 val (sdecs1', e') = loop e sdecs1
                 val (sdecs2', e'') = loop (e ++ e') sdecs2
               in
                 join t (e ++ e'') [sdecs1', sdecs2']
               end
             | _ => die ()
            )
end
