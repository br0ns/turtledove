SML = mlton
APP = SourceExplorer

.PHONY: all
all : $(APP)

.PHONY : $(APP)
$(APP) : $(APP).mlb $(APP).sml
	$(SML) $(APP).mlb



.PHONY: tests
tests: 
	@echo "Possible tests are:"
	@echo "* basis.mlb"
	@echo "* Turtledove.mlb"
	@echo "* MyLib.mlb"

.PHONY: Turtledove.mlb
Turtledove.mlb: $(APP)
	./$(APP) ../../Turtledove.mlb

.PHONY: MyLib.mlb
MyLib.mlb: $(APP)
	./$(APP) /usr/lib/mlton/sml/mylib/MyLib.mlb


.PHONY: basis.mlb
basis.mlb: $(APP)
	./$(APP) /usr/lib/mlton/sml/basis/basis.mlb




.PHONY: clean
clean :
	-rm $(APP)
