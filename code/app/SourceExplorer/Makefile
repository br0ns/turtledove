SML = mlton

.PHONY: all
all : SourceExplorer

.PHONY : SourceExplorer
SourceExplorer :
	$(SML) SourceExplorer.mlb


.PHONY: Turtledove.mlb
Turtledove.mlb:
	./SourceExplorer ../../Turtledove.mlb

.PHONY: MyLib.mlb
MyLib.mlb:
	./SourceExplorer /usr/lib/mlton/sml/mylib/MyLib.mlb


.PHONY: basis.mlb
basis.mlb:
	./SourceExplorer /usr/lib/mlton/sml/basis/basis.mlb




.PHONY: clean
clean :
	-rm SourceExplorer
