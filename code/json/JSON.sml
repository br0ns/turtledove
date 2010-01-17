structure JSON :> JSON =
struct
    datatype t = Object of t Dictionary.t
               | Array of t list
               | String of string
               | Number of real
               | Bool of bool
               | Null
    exception Parse of Report.t

    exception Parse' of string * int
    fun fail cs s = raise Parse' (s, length cs)
    fun die s = Crash.impossible ("JSON: " ^ s)

    fun skipWhitespace (c :: cs) =
        if Char.isSpace c then
            skipWhitespace cs
        else
            c :: cs
      | skipWhitespace _ = nil

    (* Invariant: All read functions skip trailing whitespace *)
    fun rValue (cs as c :: _) =
        if c = #"{" then
            rObject cs
        else if c = #"[" then
            rArray cs
        else if c = #"\"" then
            rString cs
        else if Char.contains "-0123456789" c then
            rNumber cs
        else if Char.contains "tf" c then
            rBool cs
        else if c = #"n" then
            rNull cs
        else
            fail cs "Not a value."
      | rValue _ = die "rValue"

    and rObject (#"{" :: cs) =
        let
            fun rPair cs =
                let
                    val (l, cs) = rString cs
                    val l = case l of String l => l | _ => die "rPair"
                in
                    case cs of
                        #":" :: cs =>
                        let
                            val (v, cs) = rValue (skipWhitespace cs)
                        in
                            ((l, v), cs)
                        end
                      | _ => fail cs "Expected colon in object."
                end
            fun loop cs d =
                let
                    val (p, cs) = rPair cs
                    val d = case Dictionary.insert d p of
                                SOME d => d
                              | NONE => fail cs "Identical labels in object."
                in
                    case cs of
                        #"," :: cs => loop (skipWhitespace cs) d
                      | #"}" :: cs => (d, skipWhitespace cs)
                      | _ => fail cs "Expected comma or } in object."
                end

            val (d, cs) =
                case skipWhitespace cs of
                    #"}" :: cs => (Dictionary.empty, skipWhitespace cs)
                  | cs => loop cs Dictionary.empty
        in
            (Object d, cs)
        end
      | rObject _ = die "rObject"

    and rArray (#"[" :: cs) =
        let
            fun loop cs =
                let
                    val (v, cs) = rValue cs
                in
                    case cs of
                        #"," :: cs =>
                        let
                            val (l, cs) = loop (skipWhitespace cs)
                        in
                            (v :: l, cs)
                        end
                      | #"]" :: cs => ([v], skipWhitespace cs)
                      | _ => fail cs "Expected comma or ] in array."
                end

            val (l, cs) =
                case skipWhitespace cs of
                    #"]" :: cs => (nil, skipWhitespace cs)
                  | cs => loop cs
        in
            (Array l, cs)
        end
      | rArray _ = die "rArray"

    and rString (#"\"" :: cs) =
        let
            fun charToInt c =
                if Char.isDigit c then
                    ord c - ord #"0"
                else if Char.isUpper c then
                    ord c - ord #"A" + 10
                else if Char.isLower c then
                    ord c - ord #"a" + 10
                else
                    fail cs "Expected [0-9a-fA-F] in unicode escape."

            fun charsToInt base =
                foldl (fn (c, a) => a * base + charToInt c) 0

            fun rChar (#"\\" :: c :: cs) =
                (case c of
                     #"\"" => (#"\"", cs)
                   | #"\\" => (#"\\", cs)
                   | #"b"  => (#"\b", cs)
                   | #"f"  => (#"\f", cs)
                   | #"n"  => (#"\n", cs)
                   | #"r"  => (#"\r", cs)
                   | #"t"  => (#"\t", cs)
                   | #"u"  =>
                     (case cs of
                          c1 :: c2 :: c3 :: c4 :: cs' =>
                          let
                              val c = charsToInt 16 [c1, c2, c3, c4]
                          in
                              if c > 255 then
                                  fail cs "Only unicode characters in range 0--255 supported."
                              else
                                  (chr c, cs')
                          end
                        | _ => fail cs "Unexpected end of string."
                     )
                   | _ => fail cs "Unexpected escape character."
                )
              | rChar (c :: cs) = (c, cs)
              | rChar nil = fail cs "Unexpected end of string."

            fun loop (#"\"" :: cs) = (nil, skipWhitespace cs)
              | loop cs =
                let
                    val (c, cs) = rChar cs
                    val (s, cs) = loop cs
                in
                    (c :: s, cs)
                end

            val (s, cs) = loop cs
        in
            (String (implode s), cs)
        end
      | rString _ = die "rString"

    and rNumber cs =
        let
            fun next (c :: cs) = SOME (c, cs)
              | next nil = NONE
        in
            (* Hack: Real.scan doesn't force a digit *)
            (* in front of the decimal point.        *)
            case Real.scan next cs of
                SOME (n, cs) => (Number n, skipWhitespace cs)
              | NONE => fail cs "Not a number."
        end

    and rBool (#"t" :: #"r" :: #"u" :: #"e" :: cs) = (Bool true, skipWhitespace cs)
      | rBool (#"f" :: #"a" :: #"l" :: #"s" :: #"e" :: cs) = (Bool false, skipWhitespace cs)
      | rBool cs = fail cs "Not a boolean value."

    and rNull (#"n" :: #"u" :: #"l" :: #"l" :: cs) = (Null, skipWhitespace cs)
      | rNull cs = fail cs "Not null."

    fun error (e, s, n) =
        let
            open Report
            infix ++
        in
            raise Parse (
                  text ("JSON: " ^ e) ++
                  text "in" ++
                  indent (text s) ++
                  text ("at position " ^ Int.toString n)
                  )
        end

    fun read s =
        (case rValue (explode s) of
             (v, nil) => v
           | _ => fail (explode s) "Not a JSON value."
        ) handle Parse' (e, n) => error (e, s, size s - n)
    fun readMany s =
        let
            fun loop nil = nil
              | loop cs =
                let
                    val (v, cs) = rValue cs
                in
                    v :: loop cs
                end
        in
            loop (explode s)
        end handle Parse' (e, n) => error (e, s, size s - n)

    fun write (Object d) =
        let
            fun wPair (k, v) = "\"" ^ k ^ "\": " ^ write v
            fun loop [p] = wPair p
              | loop (p :: ps) = wPair p ^ ", " ^ loop ps
              | loop nil = ""
        in
            "{" ^ loop (Dictionary.toList d) ^ "}"
        end
      | write (Array l) =
        let
            fun loop [v] = write v
              | loop (v :: vs) = write v ^ ", " ^ loop vs
              | loop nil = ""
        in
            "[" ^ loop l ^ "]"
        end
      | write (String s) = "\"" ^ s ^ "\""
      | write (Number n) =
        if n < 0.0 then
            "-" ^ Real.toString (~n)
        else
            Real.toString n
      | write (Bool b) = Bool.toString b
      | write Null = "null"
    fun writeMany nil = ""
      | writeMany [v] = write v
      | writeMany (v :: vs) = write v ^ "\n" ^ writeMany vs

    fun from (f, _) = f o read
    fun fromMany (f, _) = map f o readMany
    fun to (_, t) = write o t
    fun toMany (_, t) = writeMany o map t

    structure Converter =
    struct
        type json = t
        type 'a t = (json -> 'a) * ('a -> json)

        fun make {toJSON, fromJSON} = (fromJSON, toJSON)

        fun object (f, t) =
            (fn Object d => Dictionary.map f d
              | _ => raise Match
           , fn d => Object (Dictionary.map t d)
            )

        fun array (f, t) =
            (fn Array l => map f l
              | _ => raise Match
           , fn l => Array (map t l)
            )

        val string =
            (fn String s => s
              | _ => raise Match
           , fn s => String s
            )

        val number =
            (fn Number n => n
              | _ => raise Match
           , fn n => Number n)

        val bool =
            (fn Bool b => b
              | _ => raise Match
           , fn b => Bool b
            )

        val null =
            (fn Null => ()
              | _ => raise Match
           , fn () => Null
            )

        val json =
            (fn x => x
           , fn x => x
            )
    end
end
