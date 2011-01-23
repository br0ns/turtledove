SML = mlton


.PHONY : all
all : parsers turtledove app test

.PHONY : turtledove
turtledove :
	$(SML) Turtledove.mlb


.PHONY : test clean-test
test :
	make -C test -f Makefile all
clean-test:
	make -C test -f Makefile clean


.PHONY : app clean-app
app : 
	make -C app -f Makefile all
clean-app :
	make -C app -f Makefile clean

.PHONY : parsers clean-parsers
parsers :
	make -C parsers -f Makefile all
clean-parsers : 
	make -C parsers -F Makefile clean
