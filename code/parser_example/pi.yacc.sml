functor PiLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Pi_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open DataTypes

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\002\000\029\000\003\000\029\000\005\000\029\000\008\000\029\000\
\\009\000\029\000\011\000\029\000\000\000\
\\001\000\002\000\032\000\003\000\032\000\005\000\032\000\008\000\032\000\
\\009\000\032\000\011\000\032\000\000\000\
\\001\000\002\000\033\000\003\000\033\000\005\000\033\000\008\000\033\000\
\\009\000\033\000\011\000\033\000\000\000\
\\001\000\002\000\034\000\003\000\034\000\005\000\034\000\008\000\034\000\
\\009\000\034\000\011\000\034\000\000\000\
\\001\000\002\000\036\000\003\000\036\000\004\000\036\000\005\000\036\000\
\\007\000\036\000\008\000\036\000\009\000\036\000\010\000\036\000\
\\011\000\036\000\000\000\
\\001\000\002\000\037\000\003\000\037\000\005\000\037\000\008\000\037\000\
\\009\000\037\000\011\000\037\000\000\000\
\\001\000\002\000\038\000\003\000\038\000\005\000\038\000\008\000\038\000\
\\009\000\038\000\011\000\038\000\000\000\
\\001\000\002\000\039\000\003\000\039\000\005\000\039\000\008\000\039\000\
\\009\000\039\000\011\000\039\000\000\000\
\\001\000\002\000\040\000\003\000\040\000\005\000\040\000\008\000\040\000\
\\009\000\040\000\011\000\040\000\000\000\
\\001\000\002\000\041\000\003\000\041\000\005\000\041\000\008\000\041\000\
\\009\000\041\000\011\000\041\000\000\000\
\\001\000\002\000\023\000\011\000\022\000\000\000\
\\001\000\003\000\000\000\000\000\
\\001\000\003\000\028\000\000\000\
\\001\000\003\000\030\000\000\000\
\\001\000\003\000\031\000\005\000\008\000\008\000\007\000\009\000\006\000\000\000\
\\001\000\004\000\035\000\000\000\
\\001\000\004\000\024\000\000\000\
\\001\000\005\000\008\000\000\000\
\\001\000\005\000\008\000\008\000\007\000\009\000\006\000\000\000\
\\001\000\005\000\008\000\008\000\007\000\009\000\006\000\011\000\015\000\000\000\
\\001\000\007\000\011\000\010\000\010\000\000\000\
\"
val actionRowNumbers =
"\014\000\014\000\020\000\012\000\
\\017\000\019\000\004\000\013\000\
\\017\000\017\000\018\000\010\000\
\\008\000\003\000\006\000\009\000\
\\015\000\016\000\007\000\005\000\
\\002\000\019\000\018\000\001\000\
\\000\000\011\000"
val gotoT =
"\
\\002\000\025\000\003\000\003\000\006\000\002\000\008\000\001\000\000\000\
\\003\000\007\000\006\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\006\000\010\000\000\000\
\\004\000\012\000\006\000\002\000\008\000\011\000\000\000\
\\000\000\
\\000\000\
\\006\000\015\000\009\000\014\000\000\000\
\\001\000\018\000\005\000\017\000\006\000\016\000\000\000\
\\006\000\002\000\008\000\019\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\023\000\006\000\002\000\008\000\011\000\000\000\
\\006\000\002\000\008\000\024\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 26
val numrules = 14
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
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | IDE of unit ->  (string) | value of unit ->  (V)
 | proc of unit ->  (Proc) | pi of unit ->  (Proc list)
 | path of unit ->  (Path) | pat of unit ->  (Pat)
 | parallel of unit ->  (Proc list) | procList of unit ->  (Proc list)
 | begin of unit ->  (Pi) | abs of unit ->  (A)
end
type svalue = MlyValue.svalue
type result = Pi
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 8) => true | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 2) => true | _ => false
val showTerminal =
fn (T 0) => "CARET"
  | (T 1) => "DVBAR"
  | (T 2) => "EOF"
  | (T 3) => "EQUALS"
  | (T 4) => "IDE"
  | (T 5) => "ILLCH"
  | (T 6) => "INPUT"
  | (T 7) => "LPAR"
  | (T 8) => "NEW"
  | (T 9) => "OUTPUT"
  | (T 10) => "RPAR"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 3) $$ 
(T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
type int = Int.int
exception mlyAction of int
local open Header in
val actions = 
fn (i392:int,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.procList procList1, procList1left, 
procList1right)) :: rest671)) => let val  result = MlyValue.begin (fn
 _ => let val  (procList as procList1) = procList1 ()
 in ((Pi procList))
end)
 in ( LrTable.NT 1, ( result, procList1left, procList1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.proc proc1, _, proc1right)) :: _ :: ( _, ( 
MlyValue.pat pat1, pat1left, _)) :: rest671)) => let val  result = 
MlyValue.abs (fn _ => let val  (pat as pat1) = pat1 ()
 val  (proc as proc1) = proc1 ()
 in ((A (pat,proc)))
end)
 in ( LrTable.NT 0, ( result, pat1left, proc1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.procList procList1, _, procList1right)) :: (
 _, ( MlyValue.proc proc1, proc1left, _)) :: rest671)) => let val  
result = MlyValue.procList (fn _ => let val  (proc as proc1) = proc1
 ()
 val  (procList as procList1) = procList1 ()
 in ((proc::procList))
end)
 in ( LrTable.NT 2, ( result, proc1left, procList1right), rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.procList (fn _ => (
[]))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( MlyValue.parallel parallel1, _, parallel1right)) :: _
 :: ( _, ( MlyValue.proc proc1, proc1left, _)) :: rest671)) => let
 val  result = MlyValue.parallel (fn _ => let val  (proc as proc1) = 
proc1 ()
 val  (parallel as parallel1) = parallel1 ()
 in ((proc::parallel))
end)
 in ( LrTable.NT 3, ( result, proc1left, parallel1right), rest671)
end
|  ( 5, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.proc proc1, 
proc1left, _)) :: rest671)) => let val  result = MlyValue.parallel (fn
 _ => let val  (proc as proc1) = proc1 ()
 in ([proc])
end)
 in ( LrTable.NT 3, ( result, proc1left, RPAR1right), rest671)
end
|  ( 6, ( ( _, ( _, RPAR1left, RPAR1right)) :: rest671)) => let val  
result = MlyValue.parallel (fn _ => ([]))
 in ( LrTable.NT 3, ( result, RPAR1left, RPAR1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.path path1, path1left, path1right)) :: 
rest671)) => let val  result = MlyValue.pat (fn _ => let val  (path
 as path1) = path1 ()
 in ((Pat path))
end)
 in ( LrTable.NT 4, ( result, path1left, path1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.IDE IDE1, (IDEleft as IDE1left), (IDEright
 as IDE1right))) :: rest671)) => let val  result = MlyValue.path (fn _
 => let val  (IDE as IDE1) = IDE1 ()
 in (
(Name (IDE,fileName,
                                IDEleft,IDEright))
)
end)
 in ( LrTable.NT 5, ( result, IDE1left, IDE1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.proc proc1, _, proc1right)) :: ( _, ( 
MlyValue.path path1, _, _)) :: ( _, ( _, NEW1left, _)) :: rest671)) =>
 let val  result = MlyValue.proc (fn _ => let val  (path as path1) = 
path1 ()
 val  (proc as proc1) = proc1 ()
 in ((New (path,proc)))
end)
 in ( LrTable.NT 7, ( result, NEW1left, proc1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.value value1, _, value1right)) :: _ :: ( _,
 ( MlyValue.path path1, path1left, _)) :: rest671)) => let val  result
 = MlyValue.proc (fn _ => let val  (path as path1) = path1 ()
 val  (value as value1) = value1 ()
 in ((Output (path,value)))
end)
 in ( LrTable.NT 7, ( result, path1left, value1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.abs abs1, _, abs1right)) :: _ :: ( _, ( 
MlyValue.path path1, path1left, _)) :: rest671)) => let val  result = 
MlyValue.proc (fn _ => let val  (path as path1) = path1 ()
 val  (abs as abs1) = abs1 ()
 in ((Input (path,abs)))
end)
 in ( LrTable.NT 7, ( result, path1left, abs1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.parallel parallel1, _, parallel1right)) :: 
( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.proc (fn _ => let val  (parallel as parallel1) = parallel1 ()
 in ((Parallel parallel))
end)
 in ( LrTable.NT 7, ( result, LPAR1left, parallel1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.path path1, path1left, path1right)) :: 
rest671)) => let val  result = MlyValue.value (fn _ => let val  (path
 as path1) = path1 ()
 in ((V path))
end)
 in ( LrTable.NT 8, ( result, path1left, path1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.begin x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Pi_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun CARET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun DVBAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun IDE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.IDE (fn () => i),p1,p2))
fun ILLCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun INPUT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun NEW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun OUTPUT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
end
end
