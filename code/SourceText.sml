structure SourceText :> SourceText =
struct
    (* filename * (line lenght * line text) list *)
    type t = string option * (int * string) list

    fun die msg = Crash.impossible ("SourceText: " ^ msg)

    fun fromFile f =
        let
            val is = TextIO.openIn f
            fun lines () =
                case TextIO.inputLine is of
                    SOME l =>
                    let
                        val l' = TextUtils.untabify l
                    in
                        (size l', l') :: lines ()
                    end
                  | NONE => nil
        in
            (SOME f, lines ())
        end

    fun fromString s =
        (NONE, map (fn l =>
                       let
                           val l' = TextUtils.untabify l ^ "\n"
                       in
                           (size l', l')
                       end
                   ) (String.fields (fn c => c = #"\n") s)
        )
        
    fun reread (SOME f, st) = fromFile f
      | reread (NONE, _) = die "Can't reread SourceText constructed from string."

    fun getFileName (SOME f, _) = f
      | getFileName (NONE, _) = "<source text constructed from string>"

    fun getSource (f, ls) pl pr =
        let
            fun drop nil _ = die "Left position after end of file in getSource."
              | drop ((s, l) :: ls) n =
                if pl > n + s then
                    (* pl lies after end of this line *)
                    drop ls (n + s)
                else
                    take ((n + s - pl, String.extract (l, pl - n, NONE)) :: ls) pl
            and take nil _ = die "Right position after end of file in getSource."
              | take ((s, l) :: ls) n = 
                if pr > n + s then
                    (* pr lies after end of this line *)
                    l :: take ls (n + s)
                else
                    [String.substring (l, 0, pr - n)]
        in
            String.concat (drop ls 0)
        end

    fun getSize (_, ls) = foldl (fn ((s, _), a) => s + a) 0 ls

    fun patch st pl pr sub = Crash.unimplemented "SourceText.patch"

    fun patchLine (f, ls) l sub =
        let
            fun insert (_ :: ls) 0 = (size sub, sub) :: ls
              | insert (l :: ls) n = l :: insert ls (n - 1)
              | insert nil _ = die "No more lines in patchLine."
        in
            (f, insert ls l)
        end

    fun makeReader (f, ls) =
        let
            val ls = ref ls
            fun reader _ =
                case !ls of
                    nil => ""
                  | (_, l) :: r => (ls := r ; l)
        in
            reader
        end

    fun posToRowCol (_, ls) p =
        let
            fun loop ((n, _) :: ls) l p =
                if n > p then
                    (l, p)
                else
                    loop ls (l + 1) (p - n)
              | loop _ _ _ = die "Position outside file."
        in
            loop ls 0 p
        end
        
    fun posToString st p =
        let
            val f = getFileName st
            val (r, c) = posToRowCol st p
        in
            f ^ ":" ^ Int.toString r ^ "." ^ Int.toString c
        end

    fun showPos st = Report.text o posToString st
end
