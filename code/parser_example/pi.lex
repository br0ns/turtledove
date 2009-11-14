structure T = Tokens
type pos = int
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue,pos) token
type lexarg = string
type arg = lexarg
val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;
val badCh : string * string * int * int -> unit = fn
    (fileName,bad,line,col) =>
    TextIO.output(TextIO.stdOut,fileName^"["
          ^Int.toString line^"."^Int.toString col
          ^"] Invalid character \""^bad^"\"\n");
val eof = fn fileName => T.EOF (!lin,!col);


structure KeyWord :
sig val find : string ->
               (int * int -> (svalue,int) token) option
end =
struct
 val TableSize = 422 (* 211 *)
 val HashFactor = 5
 val hash = fn
     s => List.foldr (fn (c,v) =>
       (v*HashFactor+(ord c)) mod TableSize) 0 (explode s)
 val HashTable = Array.array(TableSize,nil) :
              (string * (int * int -> (svalue,int) token))
              list Array.array
 val add = fn
     (s,v) => let val i = hash s
              in Array.update(HashTable,i,(s,v)
                 :: (Array.sub(HashTable, i)))
              end
 val find = fn
     s => let val i = hash s
              fun f ((key,v)::r) = if s=key then SOME v
                                   else f r
                | f nil = NONE
          in f (Array.sub(HashTable, i))
          end
 val _ = (List.app add [
      ("new",         T.NEW)
     ])
end;
open KeyWord;

%%

%full
%header (functor PiLexFun(structure Tokens: Pi_TOKENS));
%arg (fileName:string);
%s PI COMMENT;

alpha         = [A-Za-z];
hexa          = "0"("x"|"X")[0-9A-Fa-f];
digit         = [0-9];
ws            = [\ \t];
eol           = ("\013\010"|"\010"|"\013");

%%

<INITIAL>{ws}* => (lin:=1; eolpos:=0;
                   YYBEGIN PI; continue ());
<PI>{ws}* => (continue ());
<PI>{eol} => (lin:=(!lin)+1;
               eolpos:=yypos+size yytext; continue ());
<PI>{alpha}+ => (case find yytext of
                    SOME v => (col:=yypos-(!eolpos);
                               v(!lin,!col))
                  | _      => (col:=yypos-(!eolpos);
                               T.IDE(yytext,!lin,!col)));
<PI>"%"    => (YYBEGIN COMMENT; continue ());
<PI>"="    => (col:=yypos-(!eolpos); T.EQUALS(!lin,!col));
<PI>"("    => (col:=yypos-(!eolpos); T.LPAR(!lin,!col));
<PI>")"    => (col:=yypos-(!eolpos); T.RPAR(!lin,!col));
<PI>"!"    => (col:=yypos-(!eolpos); T.OUTPUT(!lin,!col));
<PI>"?"    => (col:=yypos-(!eolpos); T.INPUT(!lin,!col));
<PI>"||"   => (col:=yypos-(!eolpos); T.DVBAR(!lin,!col));
<PI>.      => (col:=yypos-(!eolpos);
               badCh (fileName,yytext,!lin,!col);
               T.ILLCH(!lin,!col));
<COMMENT>{eol} => (lin:=(!lin)+1;eolpos:=yypos+size yytext;
                   YYBEGIN PI; continue ());
<COMMENT>. => (continue ());
