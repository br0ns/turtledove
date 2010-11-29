
.PHONY: all
all : sml mlb rule Parsers

.PHONY: sml mlb rule
sml :
	make -C sml -f Makefile lex yacc
mlb :
	make -C mlb -f Makefile lex yacc
rule :
	make -C rule -f Makefile lex yacc

Parsers : Parsers.mlb
	mlton Parsers.mlb

.PHONY : mlton
mlton : Parsers

.PHONY: clean
clean :
	make -C sml -f Makefile clean
	make -C mlb -f Makefile clean
	make -C rule -f Makefile clean

