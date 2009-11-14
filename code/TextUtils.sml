structure TextUtils : TextUtils =
struct
    fun decorate dec text =
        let
            fun decorate' (nil, nil) = nil
              | decorate' (nil, l) = [dec ^ (implode o rev) l]
              | decorate' (#"\n" :: cs, l) = dec ^ (implode o rev) l :: "\n" :: decorate' (cs, nil)
              | decorate' (c :: cs, l) = decorate' (cs, c :: l)
        in
            (String.concat o decorate') (explode text, nil)
        end

    fun indent level text =
        let
            fun spaces 0 = ""
              | spaces n = " " ^ spaces (n - 1)
        in
            decorate (spaces level) text
        end

    fun wordWrap length text =
        let
            fun next (w, (t, l)) =
                let
                    val w = implode (rev w)
                    val wl = size w
                in
                    case t of
                        "" => (w, wl)
                      | _  => 
                        if l + wl + 1 > length then
                            (t ^ "\n" ^ w, wl)
                        else
                            (t ^ " " ^ w, wl + l + 1)
                end
            fun wordWrap' (nil, w, t) = next (w, t)
              | wordWrap' (#" " :: r, w, t) = wordWrap' (r, nil, next (w, t))
              | wordWrap' (#"\n" :: r, w, t) =
                let
                    val (t, _) = next (w, t)
                in
                    wordWrap' (r, nil, (t, length)) (* length + 1 > length results in newline *)
                end
              | wordWrap' (c :: r, w, t) = wordWrap' (r, c :: w, t)
        in
            #1 (wordWrap' (explode text, nil, ("", 0)))
        end

    local
        fun spaces (n, a) =
            if n <= 0 then
                a
            else
                spaces (n - 1, #" " :: a)

        fun untabifyL (col, #"\t" :: rest) =
            let
                val gap = Constants.TAB_WIDTH - col mod Constants.TAB_WIDTH
            in
                spaces (gap, untabifyL (col + gap, rest))
            end
          | untabifyL (col, char :: rest) = char :: untabifyL (case char of #"\n" => 0 | _ => col + 1, rest)
          | untabifyL (_, nil) = nil
    in
        fun untabify text = implode (untabifyL (0, explode text))
    end

end
