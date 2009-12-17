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
open MLBGrammarUtils


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\007\000\002\000\030\000\003\000\030\000\004\000\006\000\
\\006\000\005\000\009\000\004\000\010\000\030\000\000\000\
\\001\000\001\000\007\000\002\000\015\000\004\000\006\000\006\000\005\000\
\\009\000\004\000\000\000\
\\001\000\001\000\007\000\002\000\024\000\004\000\006\000\006\000\005\000\
\\009\000\004\000\000\000\
\\001\000\001\000\007\000\003\000\023\000\004\000\006\000\006\000\005\000\
\\009\000\004\000\000\000\
\\001\000\001\000\007\000\003\000\025\000\004\000\006\000\006\000\005\000\
\\009\000\004\000\000\000\
\\001\000\001\000\007\000\004\000\006\000\006\000\005\000\009\000\004\000\
\\010\000\029\000\000\000\
\\001\000\003\000\027\000\000\000\
\\001\000\005\000\014\000\000\000\
\\001\000\007\000\019\000\008\000\018\000\009\000\017\000\000\000\
\\001\000\009\000\011\000\000\000\
\\001\000\010\000\000\000\000\000\
\\031\000\001\000\007\000\004\000\006\000\006\000\005\000\009\000\004\000\000\000\
\\032\000\000\000\
\\033\000\000\000\
\\034\000\000\000\
\\035\000\000\000\
\\036\000\000\000\
\\037\000\000\000\
\\038\000\000\000\
\\039\000\000\000\
\\040\000\009\000\010\000\000\000\
\"
val actionRowNumbers =
"\011\000\005\000\015\000\020\000\
\\009\000\011\000\000\000\014\000\
\\020\000\007\000\001\000\019\000\
\\008\000\011\000\013\000\018\000\
\\011\000\011\000\003\000\002\000\
\\004\000\012\000\008\000\016\000\
\\006\000\017\000\010\000"
val gotoT =
"\
\\001\000\026\000\002\000\001\000\000\000\
\\002\000\006\000\000\000\
\\000\000\
\\004\000\007\000\000\000\
\\000\000\
\\002\000\010\000\000\000\
\\002\000\006\000\000\000\
\\000\000\
\\004\000\011\000\000\000\
\\000\000\
\\002\000\006\000\000\000\
\\000\000\
\\003\000\014\000\000\000\
\\002\000\018\000\000\000\
\\000\000\
\\000\000\
\\002\000\019\000\000\000\
\\002\000\020\000\000\000\
\\002\000\006\000\000\000\
\\002\000\006\000\000\000\
\\002\000\006\000\000\000\
\\000\000\
\\003\000\024\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 27
val numrules = 12
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
type arg = SourceText.source_text
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | EOF of unit ->  (LexUtils.Comments.comments)
 | ID of unit ->  (string) | LongBId_list of unit ->  (longbid list)
 | BExp of unit ->  (bexp) | BDec of unit ->  (bdec)
 | Start of unit ->  (bdec)
end
type svalue = MlyValue.svalue
type result = bdec
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 0) => true | (T 1) => true | (T 2) => true | (T 3) => true | (T 
5) => true | (T 6) => true | (T 7) => true | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 9) => true | _ => false
val showTerminal =
fn (T 0) => "LOCAL"
  | (T 1) => "IN"
  | (T 2) => "END"
  | (T 3) => "BASIS"
  | (T 4) => "EQ"
  | (T 5) => "OPEN"
  | (T 6) => "BAS"
  | (T 7) => "LET"
  | (T 8) => "ID"
  | (T 9) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 8) => MlyValue.ID(fn () => ("<bogus>")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 
0)end
structure Actions =
struct 
type int = Int.int
exception mlyAction of int
local open Header in
val actions = 
fn (i392:int,defaultPos,stack,
    (sourceText):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.BDec BDec1, BDec1left, BDec1right)) :: 
rest671)) => let val  result = MlyValue.Start (fn _ => let val  BDec1
 = BDec1 ()
 in ( Bdec )
end)
 in ( LrTable.NT 0, ( result, BDec1left, BDec1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.BDec BDec2, _, BDec2right)) :: ( _, ( 
MlyValue.BDec BDec1, BDec1left, _)) :: rest671)) => let val  result = 
MlyValue.BDec (fn _ => let val  BDec1 = BDec1 ()
 val  BDec2 = BDec2 ()
 in ( Seq_bdec (BDec1, BDec2) )
end)
 in ( LrTable.NT 1, ( result, BDec1left, BDec2right), rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.BDec (fn _ => (
 Empty_bdec ))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.BDec BDec2, _, _
)) :: _ :: ( _, ( MlyValue.BDec BDec1, _, _)) :: ( _, ( _, LOCAL1left,
 _)) :: rest671)) => let val  result = MlyValue.BDec (fn _ => let val 
 BDec1 = BDec1 ()
 val  BDec2 = BDec2 ()
 in ( Local_bdec (BDec1, BDec2) )
end)
 in ( LrTable.NT 1, ( result, LOCAL1left, END1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.BExp BExp1, _, BExp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, BASIS1left, _)) :: rest671)) =>
 let val  result = MlyValue.BDec (fn _ => let val  (ID as ID1) = ID1
 ()
 val  (BExp as BExp1) = BExp1 ()
 in ( Basis_bdec (mkId ID, BExp) )
end)
 in ( LrTable.NT 1, ( result, BASIS1left, BExp1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.LongBId_list LongBId_list1, _, 
LongBId_list1right)) :: ( _, ( _, OPEN1left, _)) :: rest671)) => let
 val  result = MlyValue.BDec (fn _ => let val  (LongBId_list as 
LongBId_list1) = LongBId_list1 ()
 in ( Open_bdec LongBId_list )
end)
 in ( LrTable.NT 1, ( result, OPEN1left, LongBId_list1right), rest671)

end
|  ( 6, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.BDec (fn _ => let val  (ID as ID1) = ID1
 ()
 in (
 if isMLB ID then
                      Include_bdec ID
                  else
                      Source_bdec ID 
)
end)
 in ( LrTable.NT 1, ( result, ID1left, ID1right), rest671)
end
|  ( 7, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.BDec BDec1, _, _
)) :: ( _, ( _, BAS1left, _)) :: rest671)) => let val  result = 
MlyValue.BExp (fn _ => let val  (BDec as BDec1) = BDec1 ()
 in ( Dec_bexp BDec )
end)
 in ( LrTable.NT 2, ( result, BAS1left, END1right), rest671)
end
|  ( 8, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.BExp BExp1, _, _
)) :: _ :: ( _, ( MlyValue.BDec BDec1, _, _)) :: ( _, ( _, LET1left, _
)) :: rest671)) => let val  result = MlyValue.BExp (fn _ => let val  (
BDec as BDec1) = BDec1 ()
 val  (BExp as BExp1) = BExp1 ()
 in ( Let_bexp (BDec, BExp) )
end)
 in ( LrTable.NT 2, ( result, LET1left, END1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.BExp (fn _ => let val  (ID as ID1) = ID1
 ()
 in ( mkLongId ID )
end)
 in ( LrTable.NT 2, ( result, ID1left, ID1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.LongBId_list LongBId_list1, _, 
LongBId_list1right)) :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: 
rest671)) => let val  result = MlyValue.LongBId_list (fn _ => let val 
 (ID as ID1) = ID1 ()
 val  (LongBId_list as LongBId_list1) = LongBId_list1 ()
 in ( mkLongId ID :: LongBId_list )
end)
 in ( LrTable.NT 3, ( result, ID1left, LongBId_list1right), rest671)

end
|  ( 11, ( rest671)) => let val  result = MlyValue.LongBId_list (fn _
 => ( nil ))
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : MLB_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun LOCAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun BASIS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun OPEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun BAS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun EOF (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.EOF (fn () => i),p1,p2))
end
end
