val tmp_json = "[\"hej\"]"
val tmp_json1 = "{\"name\":123}"

fun die s = raise Fail s 


;



(* Tokens *)
datatype JSON_TOKENS = TOK_NONE
                     | TOK_CURLY_OPEN
                     | TOK_CURLY_CLOSE
                     | TOK_SQUARED_OPEN
                     | TOK_SQUARED_CLOSE
                     | TOK_COLON
                     | TOK_COMMA
                     | TOK_STRING_OPEN
                     | TOK_STRING_CLOSE
                     | TOK_NUMBER
                     | TOK_TRUE
                     | TOK_FALSE
                     | TOK_NULL


datatype JSON_VALUE = VAL_OBJECT of (string * JSON_VALUE) list
                    | VAL_ARRAY of string list
                    | VAL_STRING of string
                    | VAL_NUMBER of real
                    | VAL_BOOL of bool
                    | VAL_NULL

;

(* A JSON string is always escaped, so it needs to start with #"\""  *)
fun ParseString (JSON as (#"\"" :: js)) = 
    let
      fun ParseString' (#"\"" :: js, acc) = (VAL_STRING (implode (rev acc)), js)
        | ParseString'(j::js, acc) = ParseString' (js, j :: acc)
        | ParseString' ([], _) = die "Error while trying to parse a JSON string. String was not closed properly before EOS."
    in
      ParseString' (js, [])
    end
  | ParseString JSON = die ("This is not the start of a valid JSON string: " ^ (implode JSON))


(* A JSON number needs to have atleast one digit infront of the decimal point.  *)
(* However the SML Real.scan function does all the nice stuff for us, even though *)
(* it doesn't force the one digit infront of decimal points. *)
(* This might be fixed at a later point, but not now. *)
and ParseNumber (JSON) =
    let
      fun getNextChar (j :: js) = SOME (j, js)
        | getNextChar []        = NONE
    in
      case Real.scan getNextChar JSON of
        SOME (num, rest) => (VAL_NUMBER(num), rest)
      | NONE => die ("This is not the start of a valid JSON number: " ^ (implode JSON))
    end

(* *)
and ParseObject (#"{" :: js) = 
    let
      datatype expecting_type = EXP_NAME | EXP_COLON | EXP_VALUE | EXP_COMMA

      fun ParseNameValue (JSON as (#"\"" :: js), NONE) EXP_NAME =  
          (case ParseString JSON of             
             (VAL_STRING(name), js') => ParseNameValue (js', SOME (name)) EXP_COLON
           | (_ , _)               => die ("Unexpected JSON value returned when trying to parse string: " ^ (implode js))
          )
        | ParseNameValue (#":" :: js, SOME (name)) EXP_COLON = ParseNameValue (js, SOME (name)) EXP_VALUE
        | ParseNameValue (JSON as (j :: js), SOME(name)) exp_type = 
          (case (Char.isSpace (j), exp_type) of
             (true, _) => ParseNameValue (js, SOME (name)) exp_type (* Remove any whitespace *)
           | (false, EXP_VALUE) => (* Must be the start of the value type *)
            let
              val (value, js') = ParseValue(JSON)
            in
              (name, value, js')
            end
           | (_, _) => die ("Expected a JSON value when trying to parse this objects NameValue pair. Got: " ^ (implode JSON))
          )
        | ParseNameValue (JSON, _) _ = die ("Fatal error in ParseNameValue. Parsed until: " ^ (implode JSON))

      fun ParseNameValues (#"," :: js, acc) EXP_COMMA = ParseNameValues (js, acc) EXP_NAME
        | ParseNameValues (#"}" :: js, acc) EXP_COMMA = (acc, js)
        | ParseNameValues (#"}" :: js, [])  EXP_NAME  = ([], js) (* the case where the JSON object is empty *)
        | ParseNameValues (JSON as (j :: js), acc) exp_type =  
          (case (Char.isSpace(j), exp_type) of
             (true, _ ) => ParseNameValues (js,acc) exp_type (* Remove any whitespace *)
           | (false, EXP_NAME) => 
             let
               val (name, value, js') = ParseNameValue (JSON, NONE) EXP_NAME
             in
               ParseNameValues (js', (name, value) :: acc) EXP_COMMA
             end
           | (_,     _)    => die ("Expected a NameValue pair for this JSON object. Got: " ^ (implode JSON))
          )
        | ParseNameValues (JSON, acc) _  = die ("Fatal error in ParseNameValues. Parsed until: " ^ (implode JSON))

      val (obj, js') = ParseNameValues (js, []) EXP_NAME
    in
      (VAL_OBJECT (rev obj), js')

    end
  | ParseObject js = die ("This is not the start of a valid JSON object: " ^ (implode js))


and ParseArray _ = die "NOT IMPLEMENTED"


(* We enfores lover case for "true", "false" and "null" *)
and ParseValue (JSON as (#"{" :: js))                       = ParseObject (JSON)
  | ParseValue (JSON as (#"[" :: js))                       = ParseArray (JSON)
  | ParseValue (#"t" :: #"r" :: #"u" :: #"e" :: js)         = (VAL_BOOL (true) , js)
  | ParseValue (#"f" :: #"a" :: #"l" :: #"s" :: #"e" :: js) = (VAL_BOOL (false), js)
  | ParseValue (#"n" :: #"u" :: #"l" :: #"l" :: js)         = (VAL_NULL        , js)
  | ParseValue (JSON as (j :: js)) = 
    (* is space, is a number, is a string *)
    (case (Char.isSpace(j), Char.isDigit(j) orelse j = #"-", j = #"\"") of
       (true,  false, false) => ParseValue  (js)
     | (false, true,  false) => ParseNumber (JSON)
     | (false, false, true)  => ParseString (JSON) 
     | (_,     _,     _)     => die ("Error while trying to parse JSON value at: " ^ (implode JSON))
    )                                 
  | ParseValue JSON =  die ("This is not a JSON value: " ^ (implode JSON))
   
;
