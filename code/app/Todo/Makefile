SML = mlton
APP = Todo

.PHONY: all
all : $(APP)

$(APP) : $(APP).mlb $(APP).sml
	$(SML) $(APP).mlb


.PHONY: tests
tests: 
	@echo "Possible tests are:"
	@echo "* Turtledove.mlb"
	@echo "* MyLib.mlb"

.PHONY: Turtledove.mlb
Turtledove.mlb: $(APP)
	./$(APP) ../../Turtledove.mlb

.PHONY: MyLib.mlb
MyLib.mlb: $(APP)
	./$(APP) /usr/lib/mlton/sml/mylib/MyLib.mlb

.PHONY: clean
clean :
	-rm $(APP)
