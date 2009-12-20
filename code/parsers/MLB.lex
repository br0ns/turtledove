structure T = Tokens
structure C = LexUtils.Comments
structure S = LexUtils.String

type pos = int
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue, pos) token
type lexarg = SourceText.t
type arg = lexarg

fun eof st =
    if C.depth () = 0 then
        T.EOF (C.get (), ~1, ~1)
    else
        LexError.raise (st,
                        C.openedAt (),
                        "Unclosed comment")

%%

%s C S SS;
%header (functor MLBLexFun (structure Tokens : MLB_TOKENS));
%arg (st : UserDeclarations.arg);
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

<INITIAL>{id}   => ( T.ID (yytext, yypos + size yytext) );
<INITIAL>{file} => ( T.FILE (yytext, yypos + size yytext) );

<INITIAL>\"     => ( YYBEGIN S ;
                     S.clear () ;
                     continue ()
                   );
<INITIAL>"(*"   => ( YYBEGIN C ;
                     C.new yypos ;
                     continue ()
                   );
<INITIAL>.      => ( LexError.raise (st, yypos, "Illegal token") ;
                     continue ()
                   );

<C>"(*"         => ( C.inc ;
                     continue ()
                   );
<C>"*)"         => ( C.dec ;
                     (if C.depth () = 0 then
                          YYBEGIN MLB
                      else
                          ()
                     ) ;
                     continue ()
                  );
<C>.            => ( C.append yytext ;
                     continue ()
                  );
<C>\n           => ( C.append yytext ;
                     continue ()
                   );

<S>\"           => ( YYBEGIN INITIAL ;
                     let
                         val s = S.get ()
                     in
                         T.STRING (s, yypos - size s, yypos)
                     end
                   );
<S>\\a          => ( S.appendChar #"\a"; continue () );
<S>\\b          => ( S.appendChar #"\b"; continue () );
<S>\\f          => ( S.appendChar #"\f"; continue () );
<S>\\n          => ( S.appendChar #"\n"; continue () );
<S>\\r          => ( S.appendChar #"\r"; continue () );
<S>\\t          => ( S.appendChar #"\t"; continue () );
<S>\\v          => ( S.appendChar #"\v"; continue () );
<S>\\\"         => ( S.appendChar #"\\"; continue () );
<S>\\\\         => ( S.appendChar #"\""; continue () );
<S>\\\^.        => ( S.appendControlChar yytext (LexError.raise st yypos) ;
                     continue ()
                   );
<S>\\[0-9]{3}   => ( S.appendAsciiChar yytext (LexError.raise st yypos) ;
                     continue ()
                   );
<S>\\u{hexDigit}{4} 
                => ( S.appendUnicodeChar yytext (LexError.raise st yypos) ;


<S>\\{nrws}     => ( YYBEGIN F ; continue () );
<S>\\{eol}      => ( YYBEGIN F ; continue () );
<S>\\           => ( LexError.raise st yypos "Illegal string escape" ;
                     continue ()
                   );
<S>{eol}        => ( LexError.raise st yypos "Unclosed string" ;
                     continue ()
                   );
<S>" "|[\033-\126]  
                => ( S.append yytext ;
                     continue ()
                   );
<S>.            => ( LexError.raise st (yypos + 1) "Illegal character in string" ;
                     continue ()
                   );

<F>{eol}        => ( continue () );
<F>{ws}         => ( continue () );
<F>\\           => ( YYBEGIN S ;
                     continue ()
                   );
<F>.            => ( LexError.raise st yypos "Unclosed string" ;
                     continue ()
                   );
