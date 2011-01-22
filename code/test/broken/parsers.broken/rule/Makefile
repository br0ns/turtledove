SML = mlton

.PHONY : all
all: test

test : test.mlb
	$(SML) test.mlb

.PHONY : clean
clean :
	-rm test

# Below are test runs, for ease of use

.PHONY : fold
fold : test
	./test `pwd`/testRules/fold.rule
