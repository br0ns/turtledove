SML = mlton
APP = NormalForm


.PHONY : all
all: $(APP)

$(APP) : $(APP).mlb $(APP).sml
	$(SML) $(APP).mlb

.PHONY : clean
clean :
	-rm $(APP)


.PHONY: tests
tests: 
	@echo "Possible tests are:"
	@echo "* foo"
	@echo "* collatz"
	@echo "* composelist"
	@echo "* mellemsaml"
	@echo "* sumlige"
	@echo "* udvaelg"
	@echo "* mapfil"
	@echo "* evallist"


.PHONY: foo
foo: $(APP)
	./$(APP) -v files/test.mlb files/foo.sml
.PHONY: collatz
collatz: $(APP)
	./$(APP) -v files/test.mlb files/collatz.sml
.PHONY: composelist
composelist: $(APP)
	./$(APP) -v files/test.mlb files/composelist.sml
.PHONY: mellemsaml
mellemsaml: $(APP)
	./$(APP) -v files/test.mlb files/mellemsaml.sml
.PHONY: sumlige
sumlige: $(APP)
	./$(APP) -v files/test.mlb files/sumlige.sml
.PHONY: udvaelg
udvaelg: $(APP)
	./$(APP) -v files/test.mlb files/udvaelg.sml
.PHONY: mapfil
mapfil: $(APP)
	./$(APP) -v files/test.mlb files/mapfil.sml
.PHONY: evallist
evallist: $(APP)
	./$(APP) -v files/test.mlb files/evallist.sml
