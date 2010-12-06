structure T = Tokens
structure C = Source.Comments
structure S = Source.String

type pos = int
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue, pos) token
type lexarg = Source.t
type arg = lexarg

fun eof source =
    if C.depth source = 0 then
        T.EOF (~1, ~1)
    else
        Source.error source
                        (C.start source)
                        "Unclosed comment"

fun tok t s p = t (s, p, p + size s)

%%
%reject
%full
%s C S CH F;
%header (functor RuleLexFun (structure Tokens : Rule_TOKENS));
%arg (source : UserDeclarations.arg);
alphanum=[A-Za-z'_0-9]*;
alphanumId=[A-Za-z]{alphanum};
sym=[-!%&$+/:<=>?@~`^|#*]|"\\";
symId={sym}+;

sec="\194\167";
secId = {sec}[A-Z]{alphanum};
pound="\194\163";
poundId = {pound}[A-Z]{alphanum};

id={alphanumId}|{symId};
longid={id}("."{id})*;
ws=("\012"|[\t\ ])*;
nrws=("\012"|[\t\ ])+;
cr="\013";
nl="\010";
eol=({cr}{nl}|{nl}|{cr});
num=[0-9]+;
frac="."{num};
exp=[eE](~?){num}; 
real=(~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
hexDigit=[0-9a-fA-F];
hexnum={hexDigit}+;

%%
<INITIAL>{ws}  => ( continue () );
<INITIAL>{eol} => ( continue () );

<INITIAL>"_address"            => ( T.ADDRESS (yypos, yypos + 8) );
<INITIAL>"_build_const"        => ( T.BUILD_CONST (yypos, yypos + 12) );
<INITIAL>"_command_line_const" => ( T.COMMAND_LINE_CONST (yypos, yypos + 19) );
<INITIAL>"_const"              => ( T.CONST (yypos, yypos + 6) );
<INITIAL>"_export"             => ( T.EXPORT (yypos, yypos + 7) );
<INITIAL>"_import"             => ( T.IMPORT (yypos, yypos + 7) );
<INITIAL>"_overload"           => ( T.OVERLOAD (yypos, yypos + 9) );
<INITIAL>"_symbol"             => ( T.SYMBOL (yypos, yypos + 7) );
<INITIAL>"_prim"               => ( T.PRIM (yypos, yypos + 5) );
 
<INITIAL>"rule"                => ( T.RULE (yypos, yypos + 4) );
<INITIAL>"clauses"             => ( T.RULE_TYPE_CLAUSES (yypos, yypos + 7) );
<INITIAL>"expression"          => ( T.RULE_TYPE_EXPRESSION (yypos, yypos + 10) );
<INITIAL>"becomes"             => ( T.BECOMES (yypos, yypos + 7) );
<INITIAL>"self"                => ( T.SELF (yypos, yypos + 4) );

<INITIAL>{secId}               => ( tok T.META (String.extract(yytext, 2, NONE))
                                        ((* As the section character is actually
                                            two ASCII chars (one UTF-8) it is
                                            counted as such, but we only want it
                                            to be counted as one (UTF-8) *)
                                         yygone := !yygone - 1;
                                         yypos) 
                                  );
<INITIAL>{poundId}             => ( tok T.TRANS (String.extract(yytext, 2, NONE))
                                        ((* As the pound character is actually
                                            two ASCII chars (one UTF-8) it is
                                            counted as such, but we only want it
                                            to be counted as one (UTF-8) *)
                                         yygone := !yygone - 1;
                                         yypos) 
                                  );


<INITIAL>"_"                   => ( T.WILD (yypos, yypos + 1) );
<INITIAL>","                   => ( T.COMMA (yypos, yypos + 1) );
<INITIAL>"{"                   => ( T.LBRACE (yypos, yypos + 1) );
<INITIAL>"}"                   => ( T.RBRACE (yypos, yypos + 1) );
<INITIAL>"["                   => ( T.LBRACKET (yypos, yypos + 1) );
<INITIAL>"]"                   => ( T.RBRACKET (yypos, yypos + 1) );
<INITIAL>";"                   => ( T.SEMICOLON (yypos, yypos + 1) );
<INITIAL>"("                   => ( T.LPAREN (yypos, yypos + 1) );
<INITIAL>")"                   => ( T.RPAREN (yypos, yypos + 1) );
<INITIAL>"..."                 => ( T.DOTDOTDOT (yypos, yypos + 3) );
<INITIAL>"|"                   => ( T.BAR (yypos, yypos + 1) );
<INITIAL>":"                   => ( T.COLON (yypos, yypos + 1) );
<INITIAL>":>"                  => ( T.COLONGT (yypos, yypos + 1) );
<INITIAL>"="                   => ( T.EQUALOP (yypos, yypos + 1) );
<INITIAL>"#"                   => ( T.HASH (yypos, yypos + 1) );
<INITIAL>"->"                  => ( T.ARROW (yypos, yypos + 2) );
<INITIAL>"=>"                  => ( T.DARROW (yypos, yypos + 2) );
<INITIAL>"and"                 => ( T.AND (yypos, yypos + 3) );
<INITIAL>"abstype"             => ( T.ABSTYPE (yypos, yypos + 7) );
<INITIAL>"as"                  => ( T.AS (yypos, yypos + 2) );
<INITIAL>"case"                => ( T.CASE (yypos, yypos + 4) );
<INITIAL>"datatype"            => ( T.DATATYPE (yypos, yypos + 8) );
<INITIAL>"else"                => ( T.ELSE (yypos, yypos + 4) );
<INITIAL>"end"                 => ( T.END (yypos, yypos + 3) );
<INITIAL>"eqtype"              => ( T.EQTYPE (yypos, yypos + 6) );
<INITIAL>"exception"           => ( T.EXCEPTION (yypos, yypos + 9) );
<INITIAL>"do"                  => ( T.DO (yypos, yypos + 2) );
<INITIAL>"fn"                  => ( T.FN (yypos, yypos + 2) );
<INITIAL>"fun"                 => ( T.FUN (yypos, yypos + 3) );
<INITIAL>"functor"             => ( T.FUNCTOR (yypos, yypos + 7) );
<INITIAL>"handle"              => ( T.HANDLE (yypos, yypos + 6) );
<INITIAL>"if"                  => ( T.IF (yypos, yypos + 2) );
<INITIAL>"in"                  => ( T.IN (yypos, yypos + 2) );
<INITIAL>"include"             => ( T.INCLUDE (yypos, yypos + 7) );
<INITIAL>"infix"               => ( T.INFIX (yypos, yypos + 5) );
<INITIAL>"infixr"              => ( T.INFIXR (yypos, yypos + 6) );
<INITIAL>"let"                 => ( T.LET (yypos, yypos + 3) );
<INITIAL>"local"               => ( T.LOCAL (yypos, yypos + 5) );
<INITIAL>"nonfix"              => ( T.NONFIX (yypos, yypos + 6) );
<INITIAL>"of"                  => ( T.OF (yypos, yypos + 2) );
<INITIAL>"op"                  => ( T.OP (yypos, yypos + 2) );
<INITIAL>"open"                => ( T.OPEN (yypos, yypos + 4) );
<INITIAL>"raise"               => ( T.RAISE (yypos, yypos + 5) );
<INITIAL>"rec"                 => ( T.REC (yypos, yypos + 3) );
<INITIAL>"sharing"             => ( T.SHARING (yypos, yypos + 7) );
<INITIAL>"sig"                 => ( T.SIG (yypos, yypos + 3) );
<INITIAL>"signature"           => ( T.SIGNATURE (yypos, yypos + 9) );
<INITIAL>"struct"              => ( T.STRUCT (yypos, yypos + 6) );
<INITIAL>"structure"           => ( T.STRUCTURE (yypos, yypos + 9) );
<INITIAL>"then"                => ( T.THEN (yypos, yypos + 4) );
<INITIAL>"type"                => ( T.TYPE (yypos, yypos + 4) );
<INITIAL>"val"                 => ( T.VAL (yypos, yypos + 3) );
<INITIAL>"where"               => ( T.WHERE (yypos, yypos + 5) );
<INITIAL>"while"               => ( T.WHILE (yypos, yypos + 5) );
<INITIAL>"with"                => ( T.WITH (yypos, yypos + 4) );
<INITIAL>"withtype"            => ( T.WITHTYPE (yypos, yypos + 8) );
<INITIAL>"orelse"              => ( T.ORELSE (yypos, yypos + 6) );
<INITIAL>"andalso"             => ( T.ANDALSO (yypos, yypos + 7) );


<INITIAL>"'"{alphanum}? => ( tok T.TYVAR yytext yypos );
<INITIAL>{longid}       => ( case yytext of
                               "*" => T.ASTERISK (yypos, yypos + 1)
                             | _   => tok T.LONGID yytext yypos
                           );
<INITIAL>{real}         => ( tok T.REAL yytext yypos );
<INITIAL>{num}          => ( tok T.INT yytext yypos );
<INITIAL>"~"{num}       => ( tok T.INT yytext yypos );
<INITIAL>"0x"{hexnum}   => ( tok T.INT yytext yypos );
<INITIAL>"~0x"{hexnum}  => ( tok T.INT yytext yypos );
<INITIAL>"0w"{num}      => ( tok T.WORD yytext yypos );
<INITIAL>"0wx"{hexnum}  => ( tok T.WORD yytext yypos );

<INITIAL>\"     => ( YYBEGIN S ;
                     S.new source yypos ;
                     continue ()
                   );
<INITIAL>\#\"   => ( YYBEGIN CH ;
                     S.new source yypos ;
                     continue ()
                   );

<INITIAL>"(*"   => ( YYBEGIN C ;
                     C.new source (yypos + 2) ;
                     continue ()
                   );
<INITIAL>.      => ( Source.error source yypos "Illegal token" ;
                     continue ()
                   );

<C>"(*"         => ( C.inc source ;
                     continue ()
                   );
<C>"*)"         => ( C.dec source ;
                     (if C.depth source = 0 then
                        YYBEGIN INITIAL
                      else
                        ()
                     ) ;
                     continue ()
                   );
<C>.            => ( C.append source yytext ;
                     continue ()
                   );
<C>\n           => ( C.append source yytext ;
                     continue ()
                   );

<S>\"           => ( YYBEGIN INITIAL ;
                     T.STRING (S.get source, S.start source, yypos)
                   );

<S>\\a          => ( S.appendChar source #"\a"; continue () );
<S>\\b          => ( S.appendChar source #"\b"; continue () );
<S>\\f          => ( S.appendChar source #"\f"; continue () );
<S>\\n          => ( S.appendChar source #"\n"; continue () );
<S>\\r          => ( S.appendChar source #"\r"; continue () );
<S>\\t          => ( S.appendChar source #"\t"; continue () );
<S>\\v          => ( S.appendChar source #"\v"; continue () );
<S>\\\"         => ( S.appendChar source #"\\"; continue () );
<S>\\\\         => ( S.appendChar source #"\""; continue () );
<S>\\\^.        => ( S.appendControlChar source yytext
                                         (Source.error source yypos) ;
                     continue ()
                   );
<S>\\[0-9]{3}   => ( S.appendAsciiChar source yytext
                                       (Source.error source yypos) ;
                     continue ()
                   );
<S>\\u{hexDigit}{4}
                => ( S.appendUnicodeChar source yytext
                                         (Source.error source yypos) ;
                     continue ()
                   );
<S>\\{nrws}     => ( YYBEGIN F ; continue () );
<S>\\{eol}      => ( YYBEGIN F ; continue () );
<S>\\           => ( Source.error source yypos "Illegal string escape" ;
                     continue ()
                   );
<S>{eol}        => ( Source.error source yypos "Unclosed string" ;
                     continue ()
                   );
<S>" "|[\033-\126]
                => ( S.append source yytext ;
                     continue ()
                   );
<S>.            => ( Source.error source (yypos + 1) "Illegal character in string" ;
                     continue ()
                   );

<F>{eol}        => ( continue () );
<F>{ws}         => ( continue () );
<F>\\           => ( YYBEGIN S ;
                     continue ()
                   );
<F>.            => ( Source.error source yypos "Unclosed string" ;
                     continue ()
                   );

<CH>\"          => ( YYBEGIN INITIAL ;
                     let
                       val s = S.get source
                       val p = S.start source
                     in
                       if size s <> 1 then
                         Source.error source p "Character string not of length 1"
                       else
                         T.CHAR (s, p, yypos)
                     end
                   );

<CH>\\a         => ( S.appendChar source #"\a"; continue () );
<CH>\\b         => ( S.appendChar source #"\b"; continue () );
<CH>\\f         => ( S.appendChar source #"\f"; continue () );
<CH>\\n         => ( S.appendChar source #"\n"; continue () );
<CH>\\r         => ( S.appendChar source #"\r"; continue () );
<CH>\\t         => ( S.appendChar source #"\t"; continue () );
<CH>\\v         => ( S.appendChar source #"\v"; continue () );
<CH>\\\"        => ( S.appendChar source #"\\"; continue () );
<CH>\\\\        => ( S.appendChar source #"\""; continue () );
<CH>\\\^.       => ( S.appendControlChar source yytext
                                         (Source.error source yypos) ;
                     continue ()
                   );
<CH>\\[0-9]{3}  => ( S.appendAsciiChar source yytext
                                       (Source.error source yypos) ;
                     continue ()
                   );
<CH>\\u{hexDigit}{4}
                => ( S.appendUnicodeChar source yytext
                                         (Source.error source yypos) ;
                     continue ()
                   );
<CH>\\          => ( Source.error source yypos "Illegal string escape" ;
                     continue ()
                   );
<CH>{eol}       => ( Source.error source yypos "Unclosed string" ;
                     continue ()
                   );
<CH>" "|[\033-\126]
                => ( S.append source yytext ;
                     continue ()
                   );
<CH>.           => ( Source.error source (yypos + 1) "Illegal character in string" ;
                     continue ()
                   );
