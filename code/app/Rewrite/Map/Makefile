SML = mlton
APP = RewriteMap

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
	@echo "* collatz"
	@echo "* udvaelg"
	@echo "* mapfil"
	@echo "* evallist"


.PHONY: collatz
collatz: $(APP)
	./$(APP) -v files/test.mlb files/collatz.sml
.PHONY: udvaelg
udvaelg: $(APP)
	./$(APP) -v files/test.mlb files/udvaelg.sml
.PHONY: mapfil
mapfil:$(APP)
	./$(APP) -v files/test.mlb files/mapfil.sml
.PHONY: evallist
evallist: $(APP)
	./$(APP) -v files/test.mlb files/evallist.sml
