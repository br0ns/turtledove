SML = mlton

.PHONY: all
all : size

size : 
	$(SML) size.mlb

.PHONY: Size.mlb
Size.mlb :
	./size ../../Size.mlb

.PHONY: Basic.mlb
Basic.mlb :
	./size ../../Basic.mlb

.PHONY: MyLib.mlb
MyLib.mlb :
	./size /usr/lib/mlton/sml/mylib/MyLib.mlb

.PHONY: clean
clean :
	-rm size
