SML = mlton

.PHONY : all
all: NormalFormTest

NormalFormTest : NormalFormTest.mlb NormalFormTest.sml
	$(SML) NormalFormTest.mlb

.PHONY : clean
clean :
	-rm NormalFormTest


.PHONY: tests collatz composelist mellemsaml sumlige udvaelg

tests: 
	@echo "Possible tests are:"
	@echo "* collatz"
	@echo "* composelist"
	@echo "* mellemsaml"
	@echo "* sumlige"
	@echo "* udvaelg"

collatz: 
	./NormalFormTest files/test.mlb files/collatz.sml
composelist: 
	./NormalFormTest files/test.mlb files/composelist.sml
mellemsaml: 
	./NormalFormTest files/test.mlb files/mellemsaml.sml
sumlige: 
	./NormalFormTest files/test.mlb files/sumlige.sml
udvaelg: 
	./NormalFormTest files/test.mlb files/udvaelg.sml
mapfil:
	./NormalFormTest files/test.mlb files/mapfil.sml
evallist:
	./NormalFormTest files/test.mlb files/evallist.sml
