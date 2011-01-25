SML = mlton
APP = Size

.PHONY: all
all : $(APP)

$(APP) : $(APP).mlb $(APP).sml
	$(SML) $(APP).mlb


.PHONY: tests
tests: 
	@echo "Possible tests are:"
	@echo "* TotalSize.mlb"
	@echo "* Basic.mlb"
	@echo "* MyLib.mlb"
	@echo ""
	@echo "TotalSize.mlb is specifically created for"
	@echo "counting the total size of this project."

.PHONY: TotalSize.mlb
TotalSize.mlb : $(APP)
	./$(APP) ../../Size.mlb

.PHONY: Basic.mlb
Basic.mlb : $(APP)
	./$(APP) ../../Basic.mlb

.PHONY: MyLib.mlb
MyLib.mlb : $(APP)
	./$(APP) /usr/lib/mlton/sml/mylib/MyLib.mlb

.PHONY: clean
clean :
	-rm $(APP)
