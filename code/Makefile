
.PHONY : all
all : turtledove tests

.PHONY : turtledove
turtledove :
	mlton Turtledove.mlb


.PHONY : tests
tests :
	make -C test/parsers/rule -f Makefile mlton


.PHONY : parsers
parsers :
	make -C parsers -f Makefile all

.PHONY : clean-parsers
clean-parsers : 
	make -C parsers -F Makefile clean
