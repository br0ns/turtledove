SML = mlton

.PHONY: all
all : sml mlb rule Parsers

.PHONY: sml mlb rule
sml :
	make -C sml -f Makefile lex yacc
mlb :
	make -C mlb -f Makefile lex yacc
rule :
	make -C rule -f Makefile lex yacc

.PHONY: Parsers
Parsers : Parsers.mlb
	$(SML) Parsers.mlb
	# The Parsers is only to test that it compiles,so delete 
	# it again as fast (hopefully faster) as we created it.
	-rm Parsers 

.PHONY : mlton
mlton : Parsers

.PHONY: clean
clean :
	make -C sml -f Makefile clean
	make -C mlb -f Makefile clean
	make -C rule -f Makefile clean

