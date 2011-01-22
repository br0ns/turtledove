SML = mlton

.PHONY: all
all : todo

.PHONY : todo
todo :
	$(SML) todo.mlb

.PHONY: Turtledove.mlb
Turtledove.mlb:
	./todo ../../Turtledove.mlb

.PHONY: MyLib.mlb
MyLib.mlb:
	./todo /usr/lib/mlton/sml/mylib/MyLib.mlb

.PHONY: clean
clean :
	-rm todo
