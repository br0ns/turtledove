structure T = Tokens
structure C = SourceData.Comments
structure S = SourceData.String

type pos = int
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue, pos) token
type lexarg = SourceData.t
type arg = lexarg

fun fail p s = raise LexError (p, s)

fun eof data =
    if C.depth data = 0 then
      T.EOF (~1, ~1)
    else
      fail (C.start data) "Unclosed comment"

%%

%s C S F;
%header (functor MLBLexFun (structure Tokens : MLB_TOKENS));
%arg (data : UserDeclarations.arg);
alphanum=[A-Za-z'_0-9]*;
alphanumId=[A-Za-z]{alphanum};
id={alphanumId};

pathvar="$("([A-Z_][A-Z0-9_]*)")";
filename=({pathvar}|[A-Za-z0-9_.])({pathvar}|[-A-Za-z0-9_.])*;
arc=({pathvar}|{filename}|"."|"..");
relpath=({arc}"/")*;
abspath="/"{relpath};
path={relpath}|{abspath};
file={path}{filename};

ws=("\012"|[\t\ ])*;
nrws=("\012"|[\t\ ])+;
cr="\013";
nl="\010";
eol=({cr}{nl}|{nl}|{cr});

hexDigit=[0-9a-fA-F];

%%
<INITIAL>{ws}   => ( continue () );
<INITIAL>{eol}  => ( continue () );
<INITIAL>"_prim"
                => ( T.PRIM      (yypos, yypos + 4) );
<INITIAL>","    => ( T.COMMA     (yypos, yypos + 1) );
<INITIAL>";"    => ( T.SEMICOLON (yypos, yypos + 1) );
<INITIAL>"="    => ( T.EQUALOP   (yypos, yypos + 1) );
<INITIAL>"ann"  => ( T.ANN       (yypos, yypos + 3) );
<INITIAL>"and"  => ( T.AND       (yypos, yypos + 3) );
<INITIAL>"bas"  => ( T.BAS       (yypos, yypos + 3) );
<INITIAL>"basis"
                => ( T.BASIS     (yypos, yypos + 5) );
<INITIAL>"end"  => ( T.END       (yypos, yypos + 3) );
<INITIAL>"functor"
                => ( T.FUNCTOR   (yypos, yypos + 7) );
<INITIAL>"in"   => ( T.IN        (yypos, yypos + 2) );
<INITIAL>"let"  => ( T.LET       (yypos, yypos + 3) );
<INITIAL>"local"
                => ( T.LOCAL     (yypos, yypos + 5) );
<INITIAL>"open" => ( T.OPEN      (yypos, yypos + 4) );
<INITIAL>"signature"
                => ( T.SIGNATURE (yypos, yypos + 9) );
<INITIAL>"structure"
                => ( T.STRUCTURE (yypos, yypos + 9) );

<INITIAL>{id}   => ( T.ID (yytext, yypos, yypos + size yytext) );
<INITIAL>{file} => ( T.FILE (yytext, yypos, yypos + size yytext) );

<INITIAL>\"     => ( YYBEGIN S ;
                     S.new data yypos ;
                     continue ()
                   );
<INITIAL>"(*"   => ( YYBEGIN C ;
                     C.new data (yypos + 2) ;
                     continue ()
                   );
<INITIAL>.      => ( fail yypos "Illegal token" ;
                     continue ()
                   );

<C>"(*"         => ( C.inc data ;
                     continue ()
                   );
<C>"*)"         => ( C.dec data ;
                     (if C.depth data = 0 then
                          YYBEGIN INITIAL
                      else
                          ()
                     ) ;
                     continue ()
                  );
<C>.            => ( C.append data yytext ;
                     continue ()
                  );
<C>\n           => ( C.append data yytext ;
                     continue ()
                   );

<S>\"           => ( YYBEGIN INITIAL ;
                     T.STRING (S.get data, S.start data, yypos)
                   );
<S>\\a          => ( S.appendChar data #"\a"; continue () );
<S>\\b          => ( S.appendChar data #"\b"; continue () );
<S>\\f          => ( S.appendChar data #"\f"; continue () );
<S>\\n          => ( S.appendChar data #"\n"; continue () );
<S>\\r          => ( S.appendChar data #"\r"; continue () );
<S>\\t          => ( S.appendChar data #"\t"; continue () );
<S>\\v          => ( S.appendChar data #"\v"; continue () );
<S>\\\"         => ( S.appendChar data #"\\"; continue () );
<S>\\\\         => ( S.appendChar data #"\""; continue () );
<S>\\\^.        => ( S.appendControlChar data yytext
                                         (fail yypos) ;
                     continue ()
                   );
<S>\\[0-9]{3}   => ( S.appendAsciiChar data yytext
                                       (fail yypos) ;
                     continue ()
                   );
<S>\\u{hexDigit}{4}
                => ( S.appendUnicodeChar data yytext
                                         (fail yypos) ;
                     continue ()
                   );
<S>\\{nrws}     => ( YYBEGIN F ; continue () );
<S>\\{eol}      => ( YYBEGIN F ; continue () );
<S>\\           => ( fail yypos "Illegal string escape" ;
                     continue ()
                   );
<S>{eol}        => ( fail yypos "Unclosed string" ;
                     continue ()
                   );
<S>" "|[\033-\126]
                => ( S.append data yytext ;
                     continue ()
                   );
<S>.            => ( fail (yypos + 1) "Illegal character in string" ;
                     continue ()
                   );

<F>{eol}        => ( continue () );
<F>{ws}         => ( continue () );
<F>\\           => ( YYBEGIN S ;
                     continue ()
                   );
<F>.            => ( fail yypos "Unclosed string" ;
                     continue ()
                   );
