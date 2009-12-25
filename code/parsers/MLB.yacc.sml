functor MLBLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : MLB_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open MLBGrammar

fun makeSourceOrInclude file st pos =
    let
        val inc = ["mlb"]
        val src = ["ml", "sml", "sig", "fun"]
    in
        if      List.exists (fn s => String.isSuffix s file) inc then
            Include file
        else if List.exists (fn s => String.isSuffix s file) src then
            Source file
        else
            Source.lexError st pos "Unknown filetype."
    end

type comments = Source.Comments.t


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\017\000\000\000\
\\001\000\001\000\017\000\006\000\031\000\012\000\030\000\000\000\
\\001\000\004\000\000\000\000\000\
\\001\000\008\000\032\000\000\000\
\\001\000\008\000\039\000\000\000\
\\001\000\008\000\041\000\000\000\
\\001\000\009\000\024\000\000\000\
\\001\000\011\000\023\000\000\000\
\\001\000\011\000\038\000\000\000\
\\043\000\000\000\
\\044\000\000\000\
\\045\000\003\000\012\000\007\000\011\000\013\000\010\000\014\000\009\000\
\\019\000\008\000\020\000\007\000\000\000\
\\046\000\000\000\
\\047\000\000\000\
\\048\000\000\000\
\\049\000\000\000\
\\050\000\000\000\
\\051\000\000\000\
\\052\000\000\000\
\\053\000\000\000\
\\054\000\000\000\
\\055\000\000\000\
\\056\000\005\000\034\000\000\000\
\\057\000\000\000\
\\058\000\000\000\
\\059\000\000\000\
\\060\000\000\000\
\\061\000\000\000\
\\062\000\000\000\
\\063\000\001\000\017\000\000\000\
\\064\000\000\000\
\\065\000\000\000\
\"
val actionRowNumbers =
"\011\000\010\000\009\000\014\000\
\\011\000\019\000\018\000\000\000\
\\011\000\000\000\011\000\013\000\
\\028\000\017\000\029\000\031\000\
\\007\000\006\000\015\000\012\000\
\\030\000\011\000\001\000\003\000\
\\026\000\024\000\022\000\020\000\
\\011\000\011\000\016\000\021\000\
\\000\000\008\000\004\000\023\000\
\\001\000\025\000\005\000\027\000\
\\002\000"
val gotoT =
"\
\\004\000\004\000\005\000\003\000\006\000\002\000\007\000\001\000\
\\013\000\040\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\004\000\005\000\003\000\006\000\011\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\010\000\014\000\011\000\013\000\012\000\012\000\000\000\
\\004\000\004\000\005\000\003\000\006\000\016\000\007\000\001\000\000\000\
\\001\000\018\000\010\000\017\000\012\000\012\000\000\000\
\\004\000\004\000\005\000\003\000\006\000\019\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\014\000\011\000\020\000\012\000\012\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\004\000\005\000\003\000\006\000\023\000\007\000\001\000\000\000\
\\002\000\027\000\008\000\026\000\009\000\025\000\010\000\024\000\
\\012\000\012\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\031\000\000\000\
\\000\000\
\\004\000\004\000\005\000\003\000\006\000\033\000\007\000\001\000\000\000\
\\004\000\004\000\005\000\003\000\006\000\034\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\035\000\010\000\017\000\012\000\012\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\038\000\009\000\025\000\010\000\024\000\012\000\012\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 41
val numrules = 23
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = Source.t
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | FILE of unit ->  (string)
 | ID of unit ->  (string) | mlb of unit ->  (basdecs*comments)
 | id of unit ->  (basid) | basids of unit ->  (basids)
 | basid of unit ->  (basid) | basexpnode of unit ->  (basexp)
 | basexp of unit ->  (basexp) | basdecsnode of unit ->  (basdecs)
 | basdecs of unit ->  (basdecs) | basdecnode of unit ->  (basdec)
 | basdec of unit ->  (basdec) | basbinds'' of unit ->  (basbinds)
 | basbinds' of unit ->  (basexp*basbinds)
 | basbinds of unit ->  (basbinds)
end
type svalue = MlyValue.svalue
type result = basdecs*comments
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 4) => true | (T 5) => true | (T 6) => true | (T 7) => true | (T 
9) => true | (T 10) => true | (T 11) => true | (T 12) => true | (T 13)
 => true | (T 14) => true | (T 15) => true | (T 16) => true | (T 17)
 => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 2))::
(nil
,nil
 $$ (T 7) $$ (T 0) $$ (T 10))::
nil
val noShift = 
fn (T 3) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "COMMA"
  | (T 2) => "SEMICOLON"
  | (T 3) => "EOF"
  | (T 4) => "AND"
  | (T 5) => "BAS"
  | (T 6) => "BASIS"
  | (T 7) => "END"
  | (T 8) => "EQUALOP"
  | (T 9) => "FUNCTOR"
  | (T 10) => "IN"
  | (T 11) => "LET"
  | (T 12) => "LOCAL"
  | (T 13) => "OPEN"
  | (T 14) => "SIGNATURE"
  | (T 15) => "STRUCTURE"
  | (T 16) => "ANN"
  | (T 17) => "PRIM"
  | (T 18) => "FILE"
  | (T 19) => "STRING"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 0) => MlyValue.ID(fn () => ("<bogus>")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11)
 $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ 
(T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
type int = Int.int
exception mlyAction of int
local open Header in
val actions = 
fn (i392:int,defaultPos,stack,
    (source):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.basdecs basdecs1, basdecs1left, 
basdecs1right)) :: rest671)) => let val  result = MlyValue.mlb (fn _
 => let val  (basdecs as basdecs1) = basdecs1 ()
 in (basdecs, Source.Comments.get source)
end)
 in ( LrTable.NT 12, ( result, basdecs1left, basdecs1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.basdecsnode basdecsnode1, basdecsnode1left, 
basdecsnode1right)) :: rest671)) => let val  result = MlyValue.basdecs
 (fn _ => let val  (basdecsnode as basdecsnode1) = basdecsnode1 ()
 in (basdecsnode)
end)
 in ( LrTable.NT 5, ( result, basdecsnode1left, basdecsnode1right), 
rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.basdecsnode (fn _ =>
 (nil))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( MlyValue.basdecs basdecs1, _, basdecs1right)) :: ( _,
 ( _, SEMICOLON1left, _)) :: rest671)) => let val  result = 
MlyValue.basdecsnode (fn _ => let val  (basdecs as basdecs1) = 
basdecs1 ()
 in (basdecs)
end)
 in ( LrTable.NT 6, ( result, SEMICOLON1left, basdecs1right), rest671)

end
|  ( 4, ( ( _, ( MlyValue.basdecs basdecs1, _, basdecs1right)) :: ( _,
 ( MlyValue.basdec basdec1, basdec1left, _)) :: rest671)) => let val  
result = MlyValue.basdecsnode (fn _ => let val  (basdec as basdec1) = 
basdec1 ()
 val  (basdecs as basdecs1) = basdecs1 ()
 in (basdec :: basdecs)
end)
 in ( LrTable.NT 6, ( result, basdec1left, basdecs1right), rest671)

end
|  ( 5, ( ( _, ( MlyValue.basdecnode basdecnode1, basdecnode1left, 
basdecnode1right)) :: rest671)) => let val  result = MlyValue.basdec
 (fn _ => let val  (basdecnode as basdecnode1) = basdecnode1 ()
 in (basdecnode)
end)
 in ( LrTable.NT 3, ( result, basdecnode1left, basdecnode1right), 
rest671)
end
|  ( 6, ( ( _, ( MlyValue.basbinds basbinds1, _, basbinds1right)) :: (
 _, ( _, BASIS1left, _)) :: rest671)) => let val  result = 
MlyValue.basdecnode (fn _ => let val  (basbinds as basbinds1) = 
basbinds1 ()
 in (Basis basbinds)
end)
 in ( LrTable.NT 4, ( result, BASIS1left, basbinds1right), rest671)

end
|  ( 7, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.basdecs basdecs2
, _, _)) :: _ :: ( _, ( MlyValue.basdecs basdecs1, _, _)) :: ( _, ( _,
 LOCAL1left, _)) :: rest671)) => let val  result = MlyValue.basdecnode
 (fn _ => let val  basdecs1 = basdecs1 ()
 val  basdecs2 = basdecs2 ()
 in (Local (basdecs1, basdecs2))
end)
 in ( LrTable.NT 4, ( result, LOCAL1left, END1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.basids basids1, _, basids1right)) :: ( _, (
 _, OPEN1left, _)) :: rest671)) => let val  result = 
MlyValue.basdecnode (fn _ => let val  (basids as basids1) = basids1 ()
 in (Open basids)
end)
 in ( LrTable.NT 4, ( result, OPEN1left, basids1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.FILE FILE1, (FILEleft as FILE1left), 
FILE1right)) :: rest671)) => let val  result = MlyValue.basdecnode (fn
 _ => let val  (FILE as FILE1) = FILE1 ()
 in (makeSourceOrInclude FILE source FILEleft)
end)
 in ( LrTable.NT 4, ( result, FILE1left, FILE1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.STRING STRING1, (STRINGleft as STRING1left)
, STRING1right)) :: rest671)) => let val  result = MlyValue.basdecnode
 (fn _ => let val  (STRING as STRING1) = STRING1 ()
 in (makeSourceOrInclude STRING source STRINGleft)
end)
 in ( LrTable.NT 4, ( result, STRING1left, STRING1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.basbinds' basbinds'1, _, basbinds'1right))
 :: _ :: ( _, ( MlyValue.basid basid1, basid1left, _)) :: rest671)) =>
 let val  result = MlyValue.basbinds (fn _ => let val  (basid as 
basid1) = basid1 ()
 val  (basbinds' as basbinds'1) = basbinds'1 ()
 in (
let
                     val (basexp, basbinds) = basbinds'
                 in
                     (basid, basexp) :: basbinds
                 end
)
end)
 in ( LrTable.NT 0, ( result, basid1left, basbinds'1right), rest671)

end
|  ( 12, ( ( _, ( MlyValue.basbinds'' basbinds''1, _, basbinds''1right
)) :: ( _, ( MlyValue.basexp basexp1, basexp1left, _)) :: rest671)) =>
 let val  result = MlyValue.basbinds' (fn _ => let val  (basexp as 
basexp1) = basexp1 ()
 val  (basbinds'' as basbinds''1) = basbinds''1 ()
 in (basexp, basbinds'')
end)
 in ( LrTable.NT 1, ( result, basexp1left, basbinds''1right), rest671)

end
|  ( 13, ( rest671)) => let val  result = MlyValue.basbinds'' (fn _ =>
 (nil))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 14, ( ( _, ( MlyValue.basbinds basbinds1, _, basbinds1right)) :: 
( _, ( _, AND1left, _)) :: rest671)) => let val  result = 
MlyValue.basbinds'' (fn _ => let val  (basbinds as basbinds1) = 
basbinds1 ()
 in (basbinds)
end)
 in ( LrTable.NT 2, ( result, AND1left, basbinds1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.basexpnode basexpnode1, basexpnode1left, 
basexpnode1right)) :: rest671)) => let val  result = MlyValue.basexp
 (fn _ => let val  (basexpnode as basexpnode1) = basexpnode1 ()
 in (basexpnode)
end)
 in ( LrTable.NT 7, ( result, basexpnode1left, basexpnode1right), 
rest671)
end
|  ( 16, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.basdecs 
basdecs1, _, _)) :: ( _, ( _, BAS1left, _)) :: rest671)) => let val  
result = MlyValue.basexpnode (fn _ => let val  (basdecs as basdecs1) =
 basdecs1 ()
 in (Bas basdecs)
end)
 in ( LrTable.NT 8, ( result, BAS1left, END1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.basid basid1, basid1left, basid1right)) :: 
rest671)) => let val  result = MlyValue.basexpnode (fn _ => let val  (
basid as basid1) = basid1 ()
 in (Var basid)
end)
 in ( LrTable.NT 8, ( result, basid1left, basid1right), rest671)
end
|  ( 18, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.basexp basexp1,
 _, _)) :: _ :: ( _, ( MlyValue.basdecs basdecs1, _, _)) :: ( _, ( _, 
LET1left, _)) :: rest671)) => let val  result = MlyValue.basexpnode
 (fn _ => let val  (basdecs as basdecs1) = basdecs1 ()
 val  (basexp as basexp1) = basexp1 ()
 in (Let (basdecs, basexp))
end)
 in ( LrTable.NT 8, ( result, LET1left, END1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.id id1, id1left, id1right)) :: rest671)) =>
 let val  result = MlyValue.basid (fn _ => let val  (id as id1) = id1
 ()
 in (id)
end)
 in ( LrTable.NT 9, ( result, id1left, id1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.basid basid1, basid1left, basid1right)) :: 
rest671)) => let val  result = MlyValue.basids (fn _ => let val  (
basid as basid1) = basid1 ()
 in ([basid])
end)
 in ( LrTable.NT 10, ( result, basid1left, basid1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.basids basids1, _, basids1right)) :: ( _, (
 MlyValue.basid basid1, basid1left, _)) :: rest671)) => let val  
result = MlyValue.basids (fn _ => let val  (basid as basid1) = basid1
 ()
 val  (basids as basids1) = basids1 ()
 in (basid :: basids)
end)
 in ( LrTable.NT 10, ( result, basid1left, basids1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.id (fn _ => let val  (ID as ID1) = ID1 ()
 in (ID)
end)
 in ( LrTable.NT 11, ( result, ID1left, ID1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.mlb x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : MLB_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun BAS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun BASIS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun LOCAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun OPEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun SIGNATURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun STRUCTURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun ANN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun PRIM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun FILE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.FILE (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
end
end
