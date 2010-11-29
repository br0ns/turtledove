
.PHONY: all
all : 
	make -C sml -f Makefile lex yacc
	make -C mlb -f Makefile lex yacc
	make -C rule -f Makefile lex yacc
.PHONY: clean
clean :
	make -C sml -f Makefile clean
	make -C mlb -f Makefile clean
	make -C rule -f Makefile clean
